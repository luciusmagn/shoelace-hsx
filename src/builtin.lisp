(uiop:define-package #:shoelace-hsx/builtin
  (:use #:cl)
  (:import-from #:shoelace-hsx/dsl
                #:deftag))
(in-package #:shoelace-hsx/builtin)

(defmacro define-builtin-tags (&rest names)
  `(progn
     ,@(mapcan (lambda (name)
                 (list `(deftag ,name)
                       `(export ',name)))
               names)))

(define-builtin-tags
    a abbr address area article aside audio b base bdi bdo blockquote
  body br button canvas caption cite code col colgroup data datalist
  dd del details dfn dialog div dl dt em embed fieldset figcaption
  figure footer form h1 h2 h3 h4 h5 h6 head header html hr i iframe
  img input ins kbd label legend li link main |map| mark meta meter nav
  noscript object ol optgroup option output p param picture pre progress
  q rp rt ruby s samp script section select small source span strong
  style sub summary sup svg table tbody td template textarea tfoot th
  thead |time| title tr track u ul var video wbr
  <> raw!
  ;; shoelace tags
  sl-alert sl-animated-image sl-animation sl-avatar sl-badge sl-breadcrumb
  sl-breadcrumb-item sl-button sl-button-group sl-card sl-carousel sl-carousel-item
  sl-checkbox sl-color-picker sl-copy-button sl-details sl-dialog sl-divider
  sl-drawer sl-dropdown sl-form sl-format-bytes sl-format-date sl-format-number
  sl-icon sl-icon-button sl-image-comparer sl-include sl-input sl-menu
  sl-menu-item sl-menu-label sl-mutation-observer sl-option sl-popup
  sl-progress-bar sl-progress-ring sl-qr-code sl-radio sl-radio-button
  sl-radio-group sl-range sl-rating sl-relative-time sl-resize-observer
  sl-select sl-skeleton sl-spinner sl-split-panel sl-switch sl-tab sl-tab-group
  sl-tab-panel sl-tag sl-textarea sl-tooltip sl-tree sl-tree-item sl-visually-hidden)
