
BASE_URL="http://localhost:8000"
MAIN="/Main.elm"
MAIN_HTML="/Main.html"
RUNTIME="/elm-runtime.js"
IMGS="img"

PUBLISH_DIR=".publish.sh"
HOST="buzzard.garrood.me"
REMOTE_BASE_PATH="~/code/library-evac"

get() {
    mkdir -p "$PUBLISH_DIR/$IMGS"
    echo "putting things into publish dir..."
    p_indent "$MAIN"
    curl "${BASE_URL}${MAIN}"    2>/dev/null > "${PUBLISH_DIR}${MAIN_HTML}"
    p_indent "$RUNTIME"
    curl "${BASE_URL}${RUNTIME}" 2>/dev/null > "${PUBLISH_DIR}${RUNTIME}"
    for f in $IMGS/*; do
        p_indent "$f"
        cp "$f" "${PUBLISH_DIR}/$f"
    done
}

send() {
    echo "copying to remote host..."
    for f in `find "$PUBLISH_DIR" -type f`; do
        g="${f#$PUBLISH_DIR}"
        p_indent "$g"
        scp "$f" "${HOST}:${REMOTE_BASE_PATH}${g}"
    done
}

p_indent() {
    echo "   $1"
}

get
send
