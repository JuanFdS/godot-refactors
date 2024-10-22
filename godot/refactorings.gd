@tool
extends Node

@onready var mi_parser = $MiParser

func _input(event):
	if event is InputEventKey:
		if event.pressed and event.ctrl_pressed and event.keycode == KEY_DOWN:
			var code_edit: CodeEdit = EditorInterface\
								.get_script_editor().get_current_editor()\
								.find_children("", "CodeEdit", true, false)\
								.front()
			var selected_text = code_edit.get_selected_text()
			var all_text = code_edit.text
			code_edit.text = mi_parser.bajar(selected_text, all_text)
			var pos = code_edit.search(selected_text.split("\n")[0], 0, 0, 0)
			var amount_of_lines = selected_text.split("\n").size()
			var finishing_caret_line = pos.y + amount_of_lines - 1
			var finishing_caret_column = code_edit.get_line_width(finishing_caret_line)
			code_edit.select(pos.y, pos.x, finishing_caret_line, finishing_caret_column)
		if event.pressed and event.ctrl_pressed and event.keycode == KEY_UP:
			var code_edit: CodeEdit = EditorInterface\
								.get_script_editor().get_current_editor()\
								.find_children("", "CodeEdit", true, false)\
								.front()
			var selected_text = code_edit.get_selected_text()
			var all_text = code_edit.text
			code_edit.text = mi_parser.subir(selected_text, all_text)
			var pos = code_edit.search(selected_text.split("\n")[0], 0, 0, 0)
			var amount_of_lines = selected_text.split("\n").size()
			var finishing_caret_line = pos.y + amount_of_lines - 1
			var finishing_caret_column = code_edit.get_line_width(finishing_caret_line)
			code_edit.select(pos.y, pos.x, finishing_caret_line, finishing_caret_column)
