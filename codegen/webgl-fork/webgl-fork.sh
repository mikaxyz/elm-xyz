#!/usr/bin/env bash

mkdir -p generated/XYZMika
rm -r elm-stuff || true

case "${1:0}" in
	enable)
		# Symlink fork of elm-explorations/webgl
		# Assumes checkout in the root of this project:
		# git clone -b render-to-texture git@github.com:mikaxyz/webgl.git
		#
		#
		cd ~/.elm/0.19.1/packages/elm-explorations/webgl
			rm -r 1.1.3
    cd -

		ln -s ${PWD}/webgl ~/.elm/0.19.1/packages/elm-explorations/webgl/1.1.3
    cp -f codegen/webgl-fork/XYZMika.WebGLForked.elm generated/XYZMika/WebGL.elm
	;;

	*)
		cd ~/.elm/0.19.1/packages/elm-explorations/webgl
		rm 1.1.3
    cd -
    cp -f codegen/webgl-fork/XYZMika.WebGL.elm generated/XYZMika/WebGL.elm
  ;;
esac
