# ~/.bashrc

# 1. Load your shared aliases, if present.
if [ -f /e/src/devtools/autoexecdev.bashrc ]; then
    source /e/src/devtools/autoexecdev.bashrc
fi

# 2. Keep PATH tweaks together (adjust as needed for Python/Conda).
export PATH="/c/Users/ianhelle/AppData/Local/anaconda3:/c/Users/ianhelle/AppData/Local/anaconda3/Scripts:/c/Users/ianhelle/AppData/Local/anaconda3/condabin:$PATH"

# 3. Minimal prompt: just your name, a dollar, and a trailing space.
export PS1="\[\033]0;$TITLEPREFIX:$PWD\007\]\n\[\033[32m\]ianhelle \[\033[33m\]\w\[\033[36m\]\`__git_ps1\`\[\033[0m\]\n$ "