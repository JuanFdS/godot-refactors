@tool
extends Node2D

func f_oo(x, y):
	if x > y:
		return "lalala"

var as_d = 2

@export_tool_button("bar") var _bar = bar
func bar():
	pass
