#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

echo $DIR

case "${1:0}" in
	debug)
    yes | cp -f "${DIR}/Console.elm" "${DIR}/../Debug.elm"
	;;

	*)
    yes | cp -f "${DIR}/Noop.elm" "${DIR}/../Debug.elm"
  ;;
esac
