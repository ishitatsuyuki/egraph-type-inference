// Copyright 2021 Tatsuyuki Ishi <ishitatsuyuki@gmail.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// An (modified) e-graph based implementation of Hindley-Milner type inference.
// The equality relationships are first constructed, with most of the unification and deduplication
// work deferred to the end of the process. This simplifies logic and potentially improves
// efficiency.
//
// The e-graph implementation is based on:
// Max Willsey, Chandrakana Nandi, Yisu Remy Wang, Oliver Flatt, Zachary Tatlock, and Pavel Panchekha. 2021.
// Egg: Fast and extensible equality saturation. Proc. ACM Program. Lang. 5, POPL, Article 23 (January 2021), 29 pages.
// DOI:https://doi.org/10.1145/3434304
// The rebuild process has been modified to also propagate equalities like f(x) == f(y) => x == y,
// which is a rule that is unique to type inference.

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

use petgraph::unionfind::UnionFind;

use crate::ast::{Expression, Type};

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
enum InferType<'ast> {
    Name(Cow<'ast, str>),
    Function([u32; 2]),
    Placeholder(u32),
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum PendingExpression<'ast> {
    Lambda {
        arg: &'ast str,
        arg_ty: u32,
        body: Box<PendingExpression<'ast>>,
    },
    Variable(&'ast str),
    Application(Box<PendingExpression<'ast>>, Box<PendingExpression<'ast>>),
}

impl<'ast> InferType<'ast> {
    fn children(&self) -> &[u32] {
        match self {
            InferType::Function(x) => x,
            _ => &[],
        }
    }

    fn children_mut(&mut self) -> &mut [u32] {
        match self {
            InferType::Function(x) => x,
            _ => &mut [],
        }
    }
}

struct EGraph<'ast> {
    hashcons: HashMap<InferType<'ast>, u32>,
    union_find: UnionFind<u32>,
    classes: HashMap<u32, EClass<'ast>>,
    work_list: Vec<u32>,
    next_id: u32,
}

struct EClass<'ast> {
    nodes: Vec<InferType<'ast>>,
    parents: Vec<(InferType<'ast>, u32)>,
}

fn canonicalize<'ast>(union_find: &UnionFind<u32>, item: &mut InferType<'ast>) {
    for child in item.children_mut() {
        *child = union_find.find(*child);
    }
}

impl<'ast> EGraph<'ast> {
    fn new() -> Self {
        EGraph {
            hashcons: Default::default(),
            union_find: UnionFind::new(4096), // FIXME: petgraph union-find does not support dynamic growth
            classes: Default::default(),
            work_list: vec![],
            next_id: 0,
        }
    }

    fn add(&mut self, mut item: InferType<'ast>) -> u32 {
        canonicalize(&self.union_find, &mut item);
        if let Some(id) = self.hashcons.get(&item) {
            // Note that the returned Id from hashcons is not necessarily canonical.
            // Doing a find() would fix that, but we don't use the property anyway so this is left as-is.
            *id
        } else {
            let id = self.next_id;
            self.next_id += 1;
            assert!(self.classes.insert(id, EClass { nodes: vec![item.clone()], parents: vec![] }).is_none());
            for child in item.children() {
                self.classes.get_mut(child).unwrap().parents.push((item.clone(), id));
            }
            self.hashcons.insert(item, id);
            id
        }
    }

    fn union(&mut self, a: u32, b: u32) -> u32 {
        // dbg!(a, b, self.union_find.equiv(a, b));
        let a = self.union_find.find(a);
        let b = self.union_find.find(b);
        if self.union_find.union(a, b) {
            let new = self.union_find.find(a);
            let old = if new == a { b } else { a };
            self.work_list.push(new);
            let old_class = self.classes.remove(&old).unwrap();
            let new_class = self.classes.get_mut(&new).unwrap();
            new_class.nodes.extend(old_class.nodes);
            new_class.parents.extend(old_class.parents);
        }
        self.union_find.find(a)
    }

    fn rebuild(&mut self) {
        while !self.work_list.is_empty() {
            // dbg!(&self.work_list);
            let mut todo = std::mem::replace(&mut self.work_list, vec![]);
            for x in &mut todo {
                *x = self.union_find.find(*x);
            }
            todo.sort_unstable();
            todo.dedup();
            for eclass in todo {
                self.repair(eclass);
            }
        }
        self.dedup_classes();
    }

    fn repair(&mut self, eclass_id: u32) {
        // dbg!(eclass_id);
        let eclass = self.classes.get_mut(&eclass_id).unwrap();

        // Defer union operations until we finish modifying the internal structures.
        let mut to_union = Vec::new();
        let mut new_parents = HashMap::new();
        for (mut enode, parent_eclass) in eclass.parents.drain(..) {
            self.hashcons.remove(&enode);
            canonicalize(&self.union_find, &mut enode);
            if let Some(dup) = new_parents.insert(enode.clone(), parent_eclass) {
                to_union.push((dup, parent_eclass));
            }
            self.hashcons.insert(enode, parent_eclass);
        }
        // dbg!(&new_parents);
        eclass.parents.extend(new_parents);

        let mut concretes = eclass.nodes.iter().filter(|x| if let InferType::Placeholder(_) = x { false } else { true });
        if let Some(unified) = concretes.next() {
            for c in concretes {
                // dbg!(unified, c);
                match (unified, c) {
                    (InferType::Name(x), InferType::Name(y)) if x == y => {}
                    (InferType::Function(x), InferType::Function(y)) => {
                        to_union.push((x[0], y[0]));
                        to_union.push((x[1], y[1]));
                    }
                    _ => panic!("Cannot unify type {:?} and {:?}", unified, c),
                }
            }
        }

        for (x, y) in to_union {
            self.union(x, y);
        }
    }

    fn dedup_classes(&mut self) {
        for class in self.classes.values_mut() {
            for enode in &mut class.nodes {
                canonicalize(&self.union_find, enode);
            }
            class.nodes.sort_unstable();
            class.nodes.dedup();
        }
    }
}

struct InferCx<'ast> {
    egraph: EGraph<'ast>,
    name_environment: HashMap<&'ast str, u32>,
    next_placeholder: u32,
    canonical_types: HashMap<u32, InferType<'ast>>,
}

impl<'ast> InferCx<'ast> {
    fn add_to_egraph(&mut self, ty: &Type<'ast>) -> u32 {
        match ty {
            Type::Name(x) => self.egraph.add(InferType::Name(x.clone())),
            Type::Function(arg, ret) => {
                let node = InferType::Function([self.add_to_egraph(arg), self.add_to_egraph(ret)]);
                self.egraph.add(node)
            }
        }
    }

    fn placeholder(&mut self) -> u32 {
        self.next_placeholder += 1;
        self.egraph.add(InferType::Placeholder(self.next_placeholder))
    }

    fn insert_relations(&mut self, ast: &Expression<'ast>) -> (u32, PendingExpression<'ast>) {
        match ast {
            Expression::Lambda { arg, arg_ty: ty, body } => {
                let arg_ty = if let Some(ty) = ty { self.add_to_egraph(ty) } else { self.placeholder() };
                let shadowed = self.name_environment.insert(arg, arg_ty);
                let (body_ty, body_expr) = self.insert_relations(body);
                if let Some(shadowed) = shadowed {
                    self.name_environment.insert(arg, shadowed);
                } else {
                    self.name_environment.remove(arg);
                }
                let lambda_ty = self.egraph.add(InferType::Function([arg_ty, body_ty]));
                (lambda_ty, PendingExpression::Lambda {
                    arg,
                    arg_ty,
                    body: Box::new(body_expr),
                })
            }
            Expression::Variable(name) => {
                (*self.name_environment.get(name).expect("Name resolution failed"), PendingExpression::Variable(name))
            }
            Expression::Application(fun, arg) => {
                let ret_ty = self.placeholder();
                let (arg_ty, arg_expr) = self.insert_relations(arg);
                let relation = self.egraph.add(InferType::Function([arg_ty, ret_ty]));
                let (fun_ty, fun_expr) = self.insert_relations(fun);
                self.egraph.union(relation, fun_ty);
                (ret_ty, PendingExpression::Application(Box::new(fun_expr), Box::new(arg_expr)))
            }
        }
    }

    /// For each eclass, decide the type to display. Concrete types are kept as-is, placeholder
    /// types are renumbered in the process.
    fn canonicalize_types(&mut self) {
        let mut placeholder_id = 0;
        for (id, eclass) in &self.egraph.classes {
            let concretes: Vec<_> = eclass.nodes.iter().filter(|x| if let InferType::Placeholder(_) = x { false } else { true }).collect();
            let canonical = match concretes.len() {
                0 => {
                    let res = InferType::Placeholder(placeholder_id);
                    placeholder_id += 1;
                    res
                }
                1 => concretes[0].clone(),
                _ => {
                    dbg!(concretes);
                    panic!("BUG: More than one concrete types (should have been unified in rebuild())");
                }
            };
            self.canonical_types.insert(*id, canonical);
        }
        // dbg!(&self.canonical_types);
    }

    fn eclass_to_ast(&self, eclass: u32, seen: &mut HashSet<u32>) -> Type<'ast> {
        let eclass = self.egraph.union_find.find(eclass);
        if !seen.insert(eclass) {
            panic!("Recursive types are not allowed in simply-typed lambda calculus");
        }
        let res = match &self.canonical_types[&eclass] {
            InferType::Name(x) => Type::Name(x.clone()),
            InferType::Function(children) => Type::Function(Box::new(self.eclass_to_ast(children[0], seen)), Box::new(self.eclass_to_ast(children[1], seen))),
            InferType::Placeholder(i) => Type::Name(format!("'{}", i).into()),
        };
        seen.remove(&eclass);
        res
    }

    fn fill_ast(&self, reference: &PendingExpression<'ast>) -> Expression<'ast> {
        match reference {
            PendingExpression::Lambda { arg, body, arg_ty } => {
                Expression::Lambda { arg_ty: Some(self.eclass_to_ast(*arg_ty, &mut HashSet::new())), arg: arg.clone(), body: Box::new(self.fill_ast(body)) }
            }
            PendingExpression::Variable(name) => Expression::Variable(name),
            PendingExpression::Application(a, b) => Expression::Application(Box::new(self.fill_ast(a)), Box::new(self.fill_ast(b))),
        }
    }
}

pub fn infer<'ast>(ast: &Expression<'ast>) -> Expression<'ast> {
    let mut cx = InferCx {
        egraph: EGraph::new(),
        name_environment: HashMap::new(),
        next_placeholder: 0,
        canonical_types: HashMap::new(),
    };
    let root = cx.insert_relations(ast).1;
    cx.egraph.rebuild();
    cx.canonicalize_types();
    cx.fill_ast(&root)
}

