@tool
extends Control

const CodeEditWithRefactors = preload("res://code_edit_with_refactors.gd")
const REFACTOR_TOOLTIP = preload("res://addons/refactorings/refactor_tooltip.tscn")

@onready var indicador = %Indicador
@onready var mi_parser: MiParser = $MiParser
var refactor_tooltip
var time_until_tooltip: float = ProjectSettings.get_setting("gui/timers/tooltip_delay_sec.editor_hint")
var time_with_mouse_in_place: float = 0.0

func _ready():
	refactor_tooltip = REFACTOR_TOOLTIP.instantiate()
	EditorInterface.get_base_control().add_child(refactor_tooltip)
	EditorInterface.get_script_editor().editor_script_changed.connect(func(script: Script):
		if(script):
			indicador.modulate = Color.GREEN
			indicador.text = "Refactors para %s" % script.resource_path
	)

func _input(event):
	if event is InputEventKey:
		if(not EditorInterface.get_script_editor().get_current_editor()):
			refactor_tooltip.visible = false
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
		elif event.pressed and event.ctrl_pressed and event.keycode == KEY_UP:
			code_edit.text = mi_parser.subir(selected_text, all_text)
			var pos = code_edit.search(selected_text.split("\n")[0], 0, 0, 0)
			var amount_of_lines = selected_text.split("\n").size()
			var finishing_caret_line = pos.y + amount_of_lines - 1
			var finishing_caret_column = code_edit.get_line_width(finishing_caret_line)
			code_edit.select(pos.y, pos.x, finishing_caret_line, finishing_caret_column)
		elif event.pressed and event.ctrl_pressed and event.keycode == KEY_E:
			var new_text = mi_parser.toggle_export(selected_line_text)
			if new_text != selected_line_text:
				var diff_columns = new_text.length() - selected_line_text.length()
				var previous_caret_column = (code_edit as TextEdit).get_caret_column()
				code_edit.remove_line_at(selected_line)
				(code_edit as TextEdit).insert_line_at(selected_line, new_text)
				code_edit.set_caret_column(max(0, previous_caret_column + diff_columns))
				code_edit.set_caret_line(selected_line)
		elif event.pressed and event.ctrl_pressed and event.keycode == KEY_T:
			var selected_function = mi_parser.function_at_line(selected_line, all_text)
			var new_text = mi_parser.toggle_tool_button(selected_function, all_text)
			if new_text != all_text:
				var diff_lines = new_text.split("\n").size() - all_text.split("\n").size()
				var previous_caret_line = (code_edit as TextEdit).get_caret_line()
				var previous_caret_column = (code_edit as TextEdit).get_caret_column()
				code_edit.text = new_text
				code_edit.set_caret_column(previous_caret_column)
				code_edit.set_caret_line(max(0, previous_caret_line + diff_lines))
				var script: Script = EditorInterface.get_script_editor().get_current_script()
				script.source_code = code_edit.text
				ResourceSaver.save(script)
				EditorInterface.get_selection().get_selected_nodes().map(func(node: Node):
					node.notify_property_list_changed()
				)
		elif event.pressed and event.keycode == KEY_CTRL:
			refactor_tooltip.queue_free()
			refactor_tooltip = REFACTOR_TOOLTIP.instantiate()
			EditorInterface.get_base_control().add_child(refactor_tooltip)
			refactor_tooltip.appear(code_edit)

func _exit_tree():
	refactor_tooltip.queue_free()
