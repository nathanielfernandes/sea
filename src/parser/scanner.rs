use std::{fmt::Debug, hash::Hash};

#[derive(Clone, Copy)]
pub struct Span<T> {
    pub start: usize,
    pub end: usize,
    pub value: T,
}

impl<T> Hash for Span<T>
where
    T: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state)
    }
}

impl<A> Span<A> {
    pub fn default(value: A) -> Self {
        Self {
            start: 0,
            end: 0,
            value,
        }
    }

    pub fn span<B>(&self, value: B) -> Span<B> {
        Span {
            start: self.start,
            end: self.end,
            value,
        }
    }
}

impl<T> Eq for Span<T> where T: Eq {}

impl<T: Debug> Debug for Span<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

pub struct Transaction<Err> {
    pub position: usize,
    pub errors: Vec<Err>,
}

impl<T> Transaction<T> {
    pub fn new(start: usize) -> Self {
        Self {
            position: start,
            errors: Vec::new(),
        }
    }
}

pub struct Scanner<Input, Err = ()> {
    pub buffer: Vec<Input>,
    pub position: usize,

    pub transactions: Vec<Transaction<Err>>,

    pub errors: Vec<Err>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ScanResult {
    Rollback,
    Commit,
}

impl<Input: Debug, Err> Scanner<Input, Err> {
    pub fn new(buffer: Vec<Input>) -> Self {
        Self {
            buffer,
            position: 0,
            transactions: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn begin(&mut self) {
        self.transactions.push(Transaction::new(self.position));
    }

    pub fn error(&mut self, error: Err) {
        match self.transactions.last_mut() {
            Some(transaction) => {
                transaction.errors.push(error);
            }
            None => {
                self.errors.push(error);
            }
        }
    }

    pub fn rollback(&mut self) {
        let transaction = self.transactions.pop().expect("why is this empty?");
        self.position = transaction.position;
    }

    pub fn commit(&mut self) {
        let transaction = self.transactions.pop();
        self.errors
            .extend(transaction.expect("why is this empty?").errors.into_iter());
    }

    pub fn peek(&self) -> Option<&Input> {
        self.buffer.get(self.position)
    }

    pub fn back(&mut self) {
        if self.position > 0 {
            self.position -= 1;
        }
    }

    pub fn next(&mut self) -> Option<&Input> {
        self.position += 1;
        self.buffer.get(self.position)
    }

    pub fn position(&self) -> usize {
        self.position
    }
}

impl<T: PartialEq> PartialEq for Span<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

pub trait SpanValue {
    type InnerValue;
    fn value(&self) -> &Self::InnerValue;
}

impl<T> SpanValue for Span<T> {
    type InnerValue = T;
    fn value(&self) -> &Self::InnerValue {
        &self.value
    }
}

impl<Input: Debug + SpanValue<InnerValue = impl PartialEq>, Err> Scanner<Input, Err> {
    pub fn skip(&mut self, value: Input::InnerValue) {
        loop {
            let current = self.peek();
            match current {
                Some(current) if current.value() == &value => {
                    self.next();
                }
                _ => {
                    break;
                }
            }
        }
    }

    pub fn skip_all<const N: usize>(&mut self, values: [Input::InnerValue; N]) {
        loop {
            let current = self.peek();
            match current {
                Some(current) if values.contains(&current.value()) => {
                    self.next();
                }
                _ => {
                    break;
                }
            }
        }
    }
}
