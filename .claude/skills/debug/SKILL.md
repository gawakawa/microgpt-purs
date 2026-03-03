---
name: debug
description: |
  Debug PureScript code using Debug.taggedLog for logging and nix run
  executed in background with monitoring.
  Triggers: "debug", "why doesn't work", "investigate", "add logs"
user-invocable: true
model: opus
---

# PureScript Debug Workflow

## 1. Add Debug Logging

```purescript
import Debug (taggedLog)

-- Usage: taggedLog returns the value unchanged (pure)
result = taggedLog "label" someValue
```

## 2. Run in Background

Run with output capture:
```bash
nix run 2>&1 | tee /tmp/purs-debug.log
```
- Use `run_in_background: true`
- Set timeout: 120s (2 min)

## 3. Monitor Every 30 Seconds

Check output:
```bash
tail -20 /tmp/purs-debug.log
```

If no new output for 30 seconds, kill process:
```bash
pkill -f "node.*microgpt"
```

## 4. Analyze and Fix

1. Read `/tmp/purs-debug.log`
2. Identify the issue from taggedLog output
3. Fix the code
4. Remove or adjust debug logs
5. Re-run `nix run` to verify

## 5. Iterate

Repeat steps 2-4 until the issue is resolved.
