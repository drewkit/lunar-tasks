# lunar-tasks

[![Codeship Status for drewkit/lunar-tasks](https://app.codeship.com/projects/58324a06-f4d1-427b-9139-c25b8237cf96/status?branch=main)](https://app.codeship.com/projects/466130)

### To Locally Run Elm App

```
elm-live src/Main.elm --open --dir=public -- --output=public/js/main.js --debug
```

Demo Mode will operate without backend round trips, otherwise you must rely on data coming in from production db.
