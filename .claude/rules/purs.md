---
paths:
  - "**/*.purs"
---

# PureScript Code Style Rules

- Prefer `where` over `let...in` for local bindings

- Define functions as pipelines using composition (`<<<`, `>=>`, `<$>`, `>>=`)

  ```purescript
  -- Good:
  process = transform3 <<< transform2 <<< transform1
  process x = transform3 $ transform2 $ transform1 x

  -- Bad:
  process x = result
    where
    y = transform1 x
    z = transform2 y
    result = transform3 z
  ```

- Make function definitions transparent — processing logic visible at the definition site

  ```purescript
  -- Good:
  gpt params caches tok pos =
    { logits: computeLogits $ applyLayers embeddings
    , caches: updateAllCaches layers embeddings
    }
    where
    embeddings = computeEmbeddings params tok pos

  -- Bad:
  gpt params caches tok pos = logits /\ caches'
    where
    -- complex where clause...
  ```

- Avoid `#` and `<#>` — use `$` and `<$>` instead
