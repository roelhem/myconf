zmodload zsh/zutil

e() {
    local flag_eval flag_tty flag_create_frame flag_reuse_frame flag_wait flag_no_wait flag_quiet
    local flag_suppress_output flag_verbose flag_help option_major_mode option_frame_parameters
    local option_display option_parent_id option_socket_name option_server_file option_tramp

    local tmpfile outfile args out_enabled

    local usage=(
        "My zsh helper/wrapper script to open files in Emacs using 'emacsclient'."
        " "
        "Usage:  e [-h|--help]"
        "        e [-nquv] [-{c|r|t}] [-M <MODE>] {[+LINE[:COLUMN]] FILENAME}..."
        "        e [-nquv] -e EXPR..."
        " "
        "Args:   FILENAME  Path to a file or directory or '-' to read from stdin."
        "        LINE      The line position of the cursor."
        "        COLUMN    The column position of the cursor."
        "        EXPR      An Elisp expression."
        " "
        "Options:"
        "  -e, --eval                            Evaluate EXPR arguments as Elisp expressions."
        "  -M MODE, --major-mode MODE            The major mode in which to open the file."
        "  -a EDITOR, --alternate-editor EDITOR  Editor to fallback to if the server is not running"
        "                                        if EDITOR is the empty string, start Emacs in daemon"
        "                                        mode and try connecting again."
        " "
        "Frame Options:"
        "  -t, --tty                             Open a new Emacs frame on the current terminal."
        "  -c, --create-frame                    Creates a new frame if none exists, otherwise"
        "                                        use the current Emacs frame."
        "  -r, --reuse-frame                     Create a new frame if none exists, otherwise"
        "                                        use the current Emacs frame."
        "  -F ALIST, --frame-parameters=ALIST    Sets the parameters of a new frame."
        " "
        "Display Options:"
        "  -d DISPLAY, --display=DISPLAY         Visit the file in the given display."
        "  --parent-id=ID                        Open in parent window ID, via XEmbed."
        " "
        "Connection Options:"
        "  --timeout=SECONDS                     Seconds to wait before timing out."
        "  -s SOCKET, --socket-name=SOCKET       Set filename of the UNIX socket for communication."
        "  -f SERVER, --server-file=SERVER       Set filename of the TCP authentication file."
        "  -T PREFIX, --tramp=PREFIX             PREFIX to prepend to filenames sent by emacsclient"
        "                                        for locating files remotely via Tramp."
        " "
        "Miscellaneous Options:"
        "  -w, --wait                            Block output until the server returns."
        "  -n, --no-wait                         Don't wait for the server to return."
        "  -q, --quiet                           Don't display messages on success."
        "  -u, --suppress-output                 Don't display return values from the server."
        "  -v, --verbose                         Show verbose output from the helper script."
        "  -h, --help                            Display this help message."
    )


    # Parse the options.
    zparseopts -D -F -E -K -- \
        {e,-eval}=flag_eval \
        {M,--major-mode}:=option_major_mode \
        {t,-tty}=flag_tty \
        {c,-create-frame}=flag_create_frame \
        {r,-reuse-frame}=flag_reuse_frame \
        {F,-frame-parameters}:=option_frame_parameters \
        {d,-display}:=option_display \
        {-parent-id}:=option_parent_id \
        -timeout:=option_timeout \
        {s,-socket-name}:=option_socket \
        {f,-server-file}:=option_server_file \
        {T,-tramp}:=option_tramp \
        {w,-wait}=flag_wait \
        {n,-no-wait}=flag_no_wait \
        {q,-quiet}=flag_quiet \
        {u,-suppress-output}:=flag_suppress_output \
        {v,-verbose}=flag_verbose \
        {h,-help}=flag_help ||
        return 1;

    # Sends verbose output to stderr if flag_verbose is set.
    verbose() {
        if [ ! -z "$flag_verbose" ]; then
            echo "$@" 1>&2;
        fi
    }

    # Show the help output if flag_help is set.
    [[ -z "$flag_help" ]] || { print -l $usage && return 0; }

    # Derive arguments.
    if [ "$#" -eq 0 ]; then
        # Case when there are no arguments.
        if [ -t 0 ]; then
            # Use
            set -- "."
        else
            set -- "-"
        fi
    elif [[ "$#" -eq 1 ]] && [[ "$1" = +* ]]; then
        # Case where the first argument defines the line numbers.
        if [ -t 0 ]; then
            set -- "$1" "."
        else
            set -- "$1" "-"
        fi
    fi

    args=()
    for arg in "$@"; do
        if [[ -z "$tmpfile" ]] && [[ "$arg" = "-" ]]; then
            tmpfile="$(mktemp --tmpdir emacs-stdin-$USERNAME.XXXXXXX 2>/dev/null || mktemp -t emacs-stdin-$USERNAME)"
            cat - > "$tmpfile"
            verbose "Wrote stdin to '$tmpfile'."
            outfile="$tmpfile"
            args+=("$tmpfile")
        else
            args+=("$arg")
        fi
    done

    # Check if the output is a terminal.
    if [ -t 1 ]; then
        # Default to no wait unless the wait flag was set.
        if [[ -z "$flag_wait" ]]; then
            flag_no_wait="--no-wait"
        fi
    else
        out_enabled="true";
    fi

    # Call emacs client
    emacsclient \
        $flag_tty \
        $flag_create_frame \
        $flag_reuse_frame \
        $flag_no_wait \
        $flag_quiet \
        $flag_suppress_output \
        $option_frame_parameters \
        $option_display \
        $option_parent_id \
        $option_timeout \
        $option_socket \
        $option_server_file \
        $option_tramp \
        $flag_eval \
        "${args[@]}";
    exit_code="$?";

    # Send the contents of outfile to the output if outfile exists.
    if [[ -f "$outfile" ]] && [[ ! -z "$out_enabled" ]]; then
        verbose "Output contents of '$outfile' to stdout."
        cat "$outfile"
        rm "$outfile"
    fi

    # Cleanup tmpfile if it exists to remove items.
    if [ -f "$tmpfile" ]; then
        rm "$tmpfile"
    fi

    return $exit_code
}
