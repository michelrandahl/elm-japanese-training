# compile with debug flag:
`$ elm make src/Quiz.elm --debug --output app.js`

# continous linting while working:
`$ find src/ | entr -cps 'elm-analyse | grep -v INFO'`

# continous compiling (and type checking) while working:
`$ find src/ | entr -cps 'elm make src/Quiz.elm --debug --output app.js'`

# continous compile and lint and write report
`$ find src/ | entr -cps 'elm make --output=app.js --debug src/Quiz.elm && elm-analyse' 2>&1 | tee analyse.report`

# serve html and javascript using python
`$ python -m "http.server" 8000`
