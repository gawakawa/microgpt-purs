# PureScript Code Style Rules

## Pipeline-First Design

Define functions as pipelines of smaller sub-functions. Use composition (`<<<`, `>=>`, `<$>`, `>>=`) instead of sequential bindings in `where` clauses.

### Avoid: Sequential bindings in where
```purescript
-- Bad: imperative-style sequential updates
process x = result
  where
  y = transform1 x
  z = transform2 y
  result = transform3 z
```

### Prefer: Function composition
```purescript
-- Good: defined as a pipeline
process = transform3 <<< transform2 <<< transform1

-- Good: using $ for application
process x = transform3 $ transform2 $ transform1 x
```

## Transparent Function Definitions

Function definitions should reveal their processing logic at the definition site.

### Avoid
```purescript
-- Processing is hidden from the definition
gpt params caches tok pos = logits /\ caches'
  where
  -- complex where clause...
```

### Prefer
```purescript
-- Definition explains the processing
gpt params caches tok pos =
  { logits: computeLogits $ applyLayers embeddings
  , caches: updateAllCaches layers embeddings
  }
  where
  embeddings = computeEmbeddings params tok pos
```
