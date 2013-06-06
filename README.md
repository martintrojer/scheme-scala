# An interpreter of (a subset of) the Scheme programming language

Written in Scala

Copyright Martin Trojer <martin.trojer@gmail.com>

This is a toy, created as code example to the blogposts listed below.
It was never the intention create a complete Scheme implementation.
If you're interested in Scheme on the JVM, I suggest Kawa
http://www.gnu.org/software/kawa/

http://martintrojer.github.io/scala/2013/06/06/scheme-in-scala/

## Usage

```
$ sbt run
[info] Running mtscheme.repl
mtscheme v0.1
null
> (+ 1 2 3)
6
> (define (map f l) (if (not (null? l)) (cons (f (car l)) (map f (cdr l)))))
null
> (map (lambda (x) (* x x)) (list 1 2 3))
(1, 4, 9, )
>
> (define (foreach f l) (if (not (null? l)) (begin (f (car l)) (foreach f (cdr l)))))
null
> (foreach display (list 1 "foo" 2 "bar"))
1
foo
2
bar
null
>
```

## Tests

```$ sbt test```

## License

GPLv3
