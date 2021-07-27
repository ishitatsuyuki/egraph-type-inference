# 「計算の理論」後半最終課題: 単純型付きラムダ計算の型推論器

---

単純型付きラムダ計算の型推論器です。Union-Findの派生であるe-graphというデータ構造を用いて、ラムダ式から変換された型方程式を効率良く解くことができます。詳細は`src/infer.rs`を参照してください。

## 実行方法

[Rust](https://www.rust-lang.org/) をセットアップした環境で `cargo run` を実行してください。

対話型となっています。ラムダ式を入力すると、型を付けたものを出力します。文法や型に誤りがある場合、その旨を出力しますが、具体的にどのように誤っているかの表示については実装していませんので、ご了承ください。

引数の型は `lambda x: T.expr` のように指定します。返り値の型を指定する文法は用意していませんが、Hindley-Milner型推論の一意性により、多くの場合返り値を指定できなくとも適切に推論されます。また、指定された型はすべて具体的な型として解釈するため、`T`が実際には関数型であった場合はエラーとなります。

λ記号は `lambda`, `\lambda` (TeX), `λ` のいずれも扱えます。

Lispなどと違い、スペースで区切られた式はすべて式適用として解釈するため、そのようなケースにおいて`()`は省略可能です。


### 実行例

第5回課題の解答

```
> lambda x.lambda y.lambda z.x z (y z)
(lambda x: ('0 -> ('2 -> '1)).(lambda y: ('0 -> '2).(lambda z: '0.((x z) (y z)))))
> lambda x.x(lambda y.lambda z.y(lambda w.x(lambda s.lambda t.z w)))
(lambda x: (((('1 -> '0) -> '2) -> (('1 -> '2) -> '2)) -> '0).(x (lambda y: (('1 -> '0) -> '2).(lambda z: ('1 -> '2).(y (lambda w: '1.(x (lambda s: (('1 -> '0) -> '2).(lambda t: ('1 -> '2).(z w))))))))))
```

注釈を与えなくとも、型推論の一意性により課題にあった型と一致します。

---

Yコンビネータ

```
> λ f . ( λ x . f   ( x   x ) )   ( λ x . f   ( x   x ) )
thread 'main' panicked at 'Recursive types are not allowed in simply-typed lambda calculus', src/infer.rs:272:13
```

内部でグラフ構造を使っているため解決は行えますが、再帰的な型を出力しようとした段階でエラーになります。

---

無効な型の例

```
> lambda a: A.a a
thread 'main' panicked at 'Cannot unify type Function([0, 1]) and Name("A")', src/infer.rs:170:26
> lambda a: A->A.lambda b: B.a b
thread 'main' panicked at 'Cannot unify type Name("B") and Name("A")', src/infer.rs:170:26
```

## ライセンス

Apache v2 or MIT License, at your option
