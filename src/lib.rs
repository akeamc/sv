use std::{collections::VecDeque, fmt::Display, mem, num::NonZeroU8, task::Poll};

use futures::{future::BoxFuture, FutureExt, Stream};
use reqwest::{Client, IntoUrl};
use select::{document::Document, node::Node, predicate};
use serde::Serialize;

#[derive(Debug, Serialize, Clone, Copy)]
pub enum Direction {
    #[serde(rename = "upp")]
    Up,
    #[serde(rename = "ned")]
    Down,
}

#[derive(Debug, Serialize, Clone, Copy)]
pub enum Dictionary {
    #[serde(rename = "saol")]
    Saol,
}

impl Display for UnambigiousWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(no) = self.homonym {
            write!(f, "{no} ")?;
        }

        write!(f, "\x1b[92m{}\x1b[0m", self.inner)
    }
}

#[derive(Debug, Clone)]
pub struct UnambigiousWord {
    inner: String,
    homonym: Option<NonZeroU8>,
}

#[derive(Debug, Clone)]
pub struct WordLink {
    word: UnambigiousWord,
    url: String,
}

#[derive(Debug, Clone)]
struct ListPage {
    items: Vec<WordLink>,
    up: String,
    down: String,
}

async fn list(
    client: &Client,
    key: String,
    dir: Direction,
    dict: Dictionary,
) -> reqwest::Result<ListPage> {
    #[derive(Debug, Serialize)]
    struct Form {
        action: &'static str,
        #[serde(rename = "unik")]
        key: String,
        dir: Direction,
        dict: Dictionary,
    }

    let form = Form {
        action: "myprefix_scrollist",
        key,
        dir,
        dict,
    };

    let html = client
        .post("https://svenska.se/wp-admin/admin-ajax.php")
        .form(&form)
        .send()
        .await?
        .text()
        .await?;

    let doc = Document::from(html.as_str());

    let items = doc.find(predicate::Class("slank")).filter_map(|n| {
        Some(WordLink {
            word: UnambigiousWord {
                inner: n.find(predicate::Class("plain")).next()?.text(),
                homonym: n.find(predicate::Class("dig")).next()?.text().parse().ok(),
            },
            url: n.attr("href")?.to_owned(),
        })
    });

    let up = doc
        .find(predicate::Attr("dir", "upp"))
        .next()
        .unwrap()
        .attr("unik")
        .unwrap()
        .parse()
        .unwrap();
    let down = doc
        .find(predicate::Attr("dir", "ned"))
        .next()
        .unwrap()
        .attr("unik")
        .unwrap()
        .parse()
        .unwrap();

    Ok(ListPage {
        items: items.collect(),
        up,
        down,
    })
}

pub struct WordStream<'a> {
    client: &'a Client,
    key: String,
    dir: Direction,
    dict: Dictionary,

    next_page_fetch: Option<BoxFuture<'a, reqwest::Result<ListPage>>>,
    words: VecDeque<WordLink>,
}

impl Stream for WordStream<'_> {
    type Item = reqwest::Result<WordLink>;

    fn poll_next(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        let inner = self.get_mut();

        if let Some(word) = inner.words.pop_front() {
            return Poll::Ready(Some(Ok(word)));
        }

        if let Some(mut fut) = inner.next_page_fetch.take() {
            return match fut.poll_unpin(cx) {
                Poll::Ready(Ok(page)) => {
                    let mut words = page.items.into_iter();
                    let next = words.next();
                    inner.words.extend(words);
                    inner.key = match inner.dir {
                        Direction::Up => page.up,
                        Direction::Down => page.down,
                    };

                    Poll::Ready(next.map(Ok))
                }
                Poll::Ready(Err(err)) => Poll::Ready(Some(Err(err))),
                Poll::Pending => {
                    inner.next_page_fetch = Some(fut);
                    Poll::Pending
                }
            };
        }

        println!("--- {}", inner.key);
        let key = mem::take(&mut inner.key);
        let mut fut = list(inner.client, key, inner.dir, inner.dict).boxed();
        assert!(fut.poll_unpin(cx).is_pending());
        inner.next_page_fetch = Some(fut);

        Poll::Pending
    }
}

pub fn words_stream(client: &Client, key: String, dir: Direction, dict: Dictionary) -> WordStream {
    WordStream {
        client,
        key,
        dir,
        dict,
        next_page_fetch: None,
        words: VecDeque::new(),
    }
}

#[derive(Debug)]
pub enum LexemComp {
    Reference(UnambigiousWord),
    Definition(String),
    Offensive(String),
    Syntex(String),
    Occurrence(String),
    Pt(String),
}

impl Display for LexemComp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexemComp::Reference(to) => write!(f, "se {to}"),
            LexemComp::Definition(def) => write!(f, "{def}"),
            LexemComp::Offensive(note) => write!(f, "{note}"),
            LexemComp::Syntex(syntex) => write!(f, "{syntex}"),
            LexemComp::Occurrence(s) => write!(f, "{s}"),
            LexemComp::Pt(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug)]
pub struct Lexem(Vec<LexemComp>);

impl Display for Lexem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        if let Some(c) = iter.next() {
            write!(f, "{c}")?;
        }
        for c in iter {
            write!(f, " {c}")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Lemma {
    word: UnambigiousWord,
    lexemes: Vec<Lexem>,
}

impl Display for Lemma {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.word)?;
        for (i, lexem) in self.lexemes.iter().enumerate() {
            writeln!(f, "\t{}. {}", i + 1, lexem)?;
        }
        Ok(())
    }
}

fn parse_lexem(node: Node) -> Option<Lexem> {
    let comps: Vec<_> = node
        .children()
        .filter_map(|node| {
            let class = node.attr("class")?;
            match class {
                "hv" => {
                    let homonym = node
                        .find(predicate::Class("hvhomo"))
                        .next()
                        .and_then(|n| n.text().parse().ok());

                    return Some(LexemComp::Reference(UnambigiousWord {
                        homonym,
                        inner: node
                            .find(predicate::Class("hvord"))
                            .next()
                            .unwrap()
                            .find(predicate::Text)
                            .next()
                            .unwrap()
                            .text(),
                    }));
                }
                "def" => Some(LexemComp::Definition(node.text())),
                "hkom" => Some(LexemComp::Offensive(node.text())),
                "rak" | "rakxs" => None,
                "syntex" => Some(LexemComp::Syntex(node.text())),
                "pf" => None,
                "pt" => Some(LexemComp::Pt(node.text())),
                "fkom" => Some(LexemComp::Occurrence(node.text())),
                "fkom2" => {
                    // fkom2 contains syntex of preceeding fkom
                    None
                }
                _ => panic!("{}", class),
            }
        })
        .collect();

    if comps.is_empty() {
        None
    } else {
        Some(Lexem(comps))
    }
}

pub async fn lemmas(client: &Client, url: impl IntoUrl) -> reqwest::Result<Vec<Lemma>> {
    let res = client.get(url).send().await?;
    let html = res.text().await?;

    let doc = Document::from(html.as_str());

    let article = doc.find(predicate::Class("article")).next().unwrap();
    let lemmas: Vec<Lemma> = article
        .find(predicate::Class("lemma"))
        .filter_map(|node| {
            let word = UnambigiousWord {
                inner: node
                    .find(predicate::Class("grundform"))
                    .next()?
                    .text()
                    .replace('\u{00ad}', ""),
                homonym: node
                    .find(predicate::Class("homonr"))
                    .next()
                    .and_then(|n| n.text().parse().ok()),
            };

            Some(Lemma {
                word,
                lexemes: node
                    .find(predicate::Class("lexemid"))
                    .filter_map(parse_lexem)
                    .collect(),
            })
        })
        .collect();

    assert!(!lemmas.is_empty());

    Ok(lemmas)
}

// #[tokio::main]
// async fn main() -> reqwest::Result<()> {
//     let client = Client::new();

//     let words_stream = words_stream(&client, "0722315".into(), Direction::Down, Dictionary::Saol);

//     let mut lemma_stream = words_stream
//         .map_ok(|w| lemmas(&client, w.url))
//         .try_buffer_unordered(10);

//     while let Some(lemmas) = lemma_stream.try_next().await? {
//         for lemma in lemmas {
//             println!("{lemma}");
//         }
//     }
//
// Ok(())
// }
