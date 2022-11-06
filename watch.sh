# set to `true` to echo all file changes
DEBUG=false
FILE_CHANGE_BATCH_MARKER='--file-change-batch-end--'
APP=toci
if [ -n "$1" ]; then
  APP=$1
fi

p() {
  now="$(date +'%r')"
  printf "$(tput setaf 3)%s$(tput sgr0) | $(tput bold)$1$(tput sgr0)\n" "$now"
}

stop_app() {
  # Find pids (the space and number before "$APP" is to match only the executable)
  pids=$(ps -ef | grep -E "[0-9] $APP" | grep -v grep | awk '{print $2}')

  # Kill previous app
  if [ -n "$pids" ]; then
    p "Stop app"
    kill -INT $pids &>/dev/null
  fi
}

start_app() {
  p "Start app"
  dune exec $APP &
}

trap stop_app SIGINT SIGTERM

stop_app && start_app

fswatch --batch-marker="${FILE_CHANGE_BATCH_MARKER}" -0 . -e _build -e "-dump.json" -e ".yml" | xargs -0 -n 1 -I {} echo {} | while read file_path; do
  if $DEBUG; then
    p $file_path
  fi

  # run block if file change batch marker is output
  if [ "$file_path" = "$FILE_CHANGE_BATCH_MARKER" ]; then
    stop_app && start_app
  fi
done
