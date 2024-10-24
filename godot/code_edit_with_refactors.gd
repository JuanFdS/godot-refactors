@tool
extends CodeEdit

const REFACTOR_TOOLTIP = preload("res://RefactorTooltip.tscn")

var tooltip
var stuff_connected = false

func connect_stuff():
	if(stuff_connected): return
	stuff_connected = true
	symbol_validate.connect(func(symbol):
		show_tooltip(symbol)
	)

func show_tooltip(symbol):
	if(not is_instance_valid(tooltip)):
		tooltip = REFACTOR_TOOLTIP.instantiate()
		add_child(tooltip)
	tooltip.visible = true
	tooltip.position = get_local_mouse_pos()
	tooltip.text = symbol
