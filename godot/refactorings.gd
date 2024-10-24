@tool
extends Node

const CodeEditWithRefactors = preload("res://code_edit_with_refactors.gd")

@onready var mi_parser: MiParser = $MiParser

func _input(event):
	if event is InputEventKey:
		if(not EditorInterface.get_script_editor().get_current_editor()):
			return

		var code_edit: CodeEdit = EditorInterface\
								.get_script_editor().get_current_editor()\
								.find_children("", "CodeEdit", true, false)\
								.front()
		var selected_text = code_edit.get_selected_text()
		var all_text = code_edit.text
		var selected_line: int = code_edit.get_caret_line()
		var selected_line_text: String = code_edit.get_line(selected_line)
		if event.pressed and event.ctrl_pressed and event.keycode == KEY_DOWN:
			code_edit.text = mi_parser.bajar(selected_text, all_text)
			var pos = code_edit.search(selected_text.split("\n")[0], 0, 0, 0)
			var amount_of_lines = selected_text.split("\n").size()
			var finishing_caret_line = pos.y + amount_of_lines - 1
			var finishing_caret_column = code_edit.get_line_width(finishing_caret_line)
			code_edit.select(pos.y, pos.x, finishing_caret_line, finishing_caret_column)
		if event.pressed and event.ctrl_pressed and event.keycode == KEY_UP:
			code_edit.text = mi_parser.subir(selected_text, all_text)
			var pos = code_edit.search(selected_text.split("\n")[0], 0, 0, 0)
			var amount_of_lines = selected_text.split("\n").size()
			var finishing_caret_line = pos.y + amount_of_lines - 1
			var finishing_caret_column = code_edit.get_line_width(finishing_caret_line)
			code_edit.select(pos.y, pos.x, finishing_caret_line, finishing_caret_column)
		if event.pressed and event.ctrl_pressed and event.keycode == KEY_E:
			var new_text = mi_parser.toggle_export(selected_line_text)
			if new_text != selected_line_text:
				var diff_columns = new_text.length() - selected_line_text.length()
				var previous_caret_column = (code_edit as TextEdit).get_caret_column()
				code_edit.remove_line_at(selected_line)
				(code_edit as TextEdit).insert_line_at(selected_line, new_text)
				code_edit.set_caret_column(max(0, previous_caret_column + diff_columns))
				code_edit.set_caret_line(selected_line)
		if event.pressed and event.ctrl_pressed and event.keycode == KEY_T:
			var new_text = mi_parser.toggle_tool_button(selected_text, all_text)
			if new_text != all_text:
				code_edit.text = new_text
				var pos = code_edit.search(selected_text.split("\n")[0], 0, 0, 0)
				var amount_of_lines = selected_text.split("\n").size()
				var finishing_caret_line = pos.y + amount_of_lines - 1
				var finishing_caret_column = code_edit.get_line_width(finishing_caret_line)
				code_edit.select(pos.y, pos.x, finishing_caret_line, finishing_caret_column)
				var script: Script = EditorInterface.get_script_editor().get_current_script()
				script.source_code = code_edit.text
				ResourceSaver.save(script)
				EditorInterface.get_selection().get_selected_nodes().map(func(node: Node):
					node.notify_property_list_changed()
				)
