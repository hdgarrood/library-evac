
BASE_URL="http://localhost:8000"
MAIN="/Main.elm"
MAIN_HTML="/Main.html"
RUNTIME="/elm-runtime.js"
PUBLISH_DIR=".publish.sh"
HOST="buzzard.garrood.me"
REMOTE_BASE_PATH="~/code/library-evac"

get() {
    mkdir -p "$PUBLISH_DIR"
    curl "${BASE_URL}${MAIN}"    > "${PUBLISH_DIR}${MAIN_HTML}"
    curl "${BASE_URL}${RUNTIME}" > "${PUBLISH_DIR}${RUNTIME}"
}

send() {
    for f in "$MAIN_HTML" "$RUNTIME"; do
        scp "${PUBLISH_DIR}${f}" "${HOST}:${REMOTE_BASE_PATH}${f}"
    done
}

get
send
