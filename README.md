# org-id-plus

The goal of this library is to provide a way to generate "semantic based" IDs for org-mode headlines.

## How to install

This is an example of how to install this package using `use-package` and `straight`:

```emacs-lisp
  (use-package org-id+
    :straight (org-id+ :repo "seblemaguer/org-id-plus" :type git :host github)
    :commands (org-id+-add-ids-to-headlines-in-file org-id+-add-id-to-current-headline))
```

## Usage

This packages is composed of the following
  - `org-id+-add-id-to-current-headline` to generate the ID property for the current headline
  - `org-id+-add-custom-id-to-current-headline` to generate the CUSTOM_ID property for the current headline
  - `org-id+-add-ids-to-headlines-in-file` to generate the ID property for **all** headlines of the current file
  - `org-id+-add-custom-ids-to-headlines-in-file` to generate the CUSTOM_ID property for **all** headlines of the current file
