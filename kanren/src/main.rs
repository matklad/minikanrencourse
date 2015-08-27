use std::rc::Rc;
use std::mem;

trait FnBox<R> {
    fn call_box(self: Box<Self>) -> R;
}

impl<R, F: FnOnce() -> R> FnBox<R> for F {
    fn call_box(self: Box<F>) -> R {
        (*self)()
    }
}

#[derive(Debug, Clone)]
enum List<T> {
    Nil,
    Cons(T, Rc<List<T>>)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Var(u64);

#[derive(Debug, PartialEq, Eq, Clone)]
enum Binding {
    Var(Var),
    Num(u64)
}

trait IntoBinding {
    fn into_binding(&self) -> Binding;
}

impl IntoBinding for Var {
    fn into_binding(&self) -> Binding { Binding::Var(*self) }
}

impl IntoBinding for u64 {
    fn into_binding(&self) -> Binding { Binding::Num(*self) }
}


type Substs = List<(Var, Binding)>;

impl Substs {
    fn find(&self, u: Var) -> Option<&Binding> {
        match self {
            &List::Nil => None,
            &List::Cons((v, ref b), ref tail) => if v == u {
                Some(b)
            } else {
                tail.find(u)
            }
        }
    }

    fn walk(&self, u: &Binding) -> Binding {
        match u {
            &Binding::Var(u) => match self.find(u) {
                None => Binding::Var(u),
                Some(b) => self.walk(b)
            },
            _ => u.clone()
        }
    }

    fn extend(&self, u: Var, v: Binding) -> Substs {
        List::Cons((u, v), Rc::new(self.clone()))
    }
}

fn unify(u: &Binding, v: &Binding, s: &Substs) -> Option<Substs> {
    let u = s.walk(u);
    let v = s.walk(v);
    match (u, v) {
        (Binding::Var(u), Binding::Var(v)) if u == v => Some(s.clone()),
        (Binding::Var(u), v) => Some(s.extend(u, v)),
        (u, Binding::Var(v)) => Some(s.extend(v, u)),
        // TODO: Pairs
        (u, v) => if u == v { Some(s.clone()) } else { None }
    }
}

enum Stream<T> {
    Empty,
    Thunk(Box<FnBox<Stream<T>>>),
    Cons(T, Box<Stream<T>>)
}

impl<T> Stream<T> {
    fn unit(t: T) -> Stream<T> {
        Stream::Cons(t, Box::new(Stream::Empty))
    }

    fn to_vec(self, limit: usize) -> Vec<T> {
        let mut result = Vec::new();
        let mut current = self;
        for i in 0..limit {
            match current {
                Stream::Empty => {},
                Stream::Thunk(ref f) => {
                    // current = f.call_box();
                },
                Stream::Cons(x, mut next) => {
                    result.push(x);
                    let next = mem::replace(&mut *next, Stream::Empty);
                    current = next
                }
            }
        }

        return result
    }
}

#[derive(Debug)]
struct State {
    bindings: Substs,
    fresh: u64,
}

const InitialState: State = State {
    bindings: List::Nil,
    fresh: 0
};

type Goal = Rc<Fn(&State) -> Stream<State>>;

fn eq<U, V>(u: U, v: V) -> Goal
    where U: IntoBinding, V: IntoBinding
{
    let u = u.into_binding();
    let v = v.into_binding();
    Rc::new(move |state| {
        if let Some(b) = unify(&u, &v, &state.bindings) {
            Stream::unit(State {
                bindings: b,
                fresh: state.fresh
            })
        } else {
            Stream::Empty
        }
    })
}

fn fresh<F>(f: F) -> Goal
    where F: Fn(Var) -> Goal, F: 'static
{
    Rc::new(move |state| {
        let var = Var(state.fresh);
        let new_state = State {
            bindings: state.bindings.clone(),
            fresh: state.fresh + 1
        };
        f(var)(&new_state)
    })
}

fn disj(g1: Goal, g2: Goal) -> Goal {
    Rc::new(move |state| {
        mplus(g1(state), g2(state))
    })
}

fn conj(g1: Goal, g2: Goal) -> Goal {
    Rc::new(move |state| {
        bind(g1(state), g2.clone())
    })
}

fn mplus(mut s1: Stream<State>, s2: Stream<State>) -> Stream<State> {
    match s1 {
        Stream::Empty => s2,
        Stream::Thunk(f) => Stream::Thunk(Box::new(move || {
            let f: Box<_> = f;
            let s: Stream<State> = f.call_box();
            mplus(s2, s)
        })),
        Stream::Cons(l, mut r) =>  {
            let tmp = mem::replace(&mut *r, Stream::Empty);
            mem::replace(&mut *r, mplus(tmp, s2));
            Stream::Cons(l, r)
        }
    }
}

fn bind(s: Stream<State>, g: Goal) -> Stream<State>
    // where F: Fn(&State) -> Stream<State>, F: 'static
{
    match s {
        Stream::Empty => Stream::Empty,
        Stream::Thunk(f) => Stream::Thunk(Box::new(move || {
            bind(f.call_box(), g)
        })),
        Stream::Cons(l, mut r) => {
            let tmp = mem::replace(&mut *r, Stream::Empty);
            mplus(g(&l), bind(tmp, g))
        }
    }
}

fn main() {
    let g = conj(
        fresh(|a| eq(a, 7)),
        fresh(|b| disj(
            eq(b, 5),
            eq(b, 6)
            ))
        );
    let result = g(&InitialState);
    println!("{:#?}", result.to_vec(3));
}
