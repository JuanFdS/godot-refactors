@tool
extends Node2D

var asd = 2

func foo():
	pass

@export_tool_button("bar") var _bar = bar
func bar():
	pass
