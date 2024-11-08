@tool
class_name Refactorings extends Control

const CodeEditWithRefactors = preload("res://code_edit_with_refactors.gd")
const REFACTOR_TOOLTIP = preload("res://addons/refactorings/refactor_tooltip.tscn")

@onready var indicador = %Indicador
@onready var mi_parser = $MiParser
var refactor_tooltip
var detected_errors = []

func _ready():
	%Error.pressed.connect(func():
		var code_edit = _code_edit()
		var caret_idx = 0
		for error in detected_errors:
			var pos = code_edit.search(error, 0, 0, 0)
			code_edit.set_line_background_color(pos.y, Color.RED)
			code_edit.caret_changed.connect(func():
				code_edit.set_line_background_color(pos.y, Color.TRANSPARENT),
				CONNECT_ONE_SHOT
			)
			code_edit.text_changed.connect(func():
				code_edit.set_line_background_color(pos.y, Color.TRANSPARENT),
				CONNECT_ONE_SHOT
			)
	)
	refactor_tooltip = REFACTOR_TOOLTIP.instantiate()
	EditorInterface.get_base_control().add_child(refactor_tooltip)
	EditorInterface.get_script_editor().editor_script_changed.connect(func(script: Script):
		var code_edit = _code_edit()
		parse_script(code_edit)
		if(not code_edit.gui_input.is_connected(self.handle_input_on_code_edit)):
			code_edit.gui_input.connect(self.handle_input_on_code_edit)
		if(not code_edit.text_changed.is_connected(self.on_code_text_changed)):
			code_edit.text_changed.connect(self.on_code_text_changed.bind(code_edit))
	)

func on_code_text_changed(code_edit):
	parse_script(code_edit)

func parse_script(code_edit: CodeEdit):
	if code_edit != _code_edit():
		return
	var errors = mi_parser.try_parse_program(code_edit.text)
	match errors:
		[]:
			%Error.visible = false
			%OK.visible = true
		_:
			%Error.visible = true
			%OK.visible = false
	detected_errors = errors

func handle_input_on_code_edit(event: InputEvent):
	if event is InputEventMouseButton:
		if event.button_index == MOUSE_BUTTON_LEFT and event.is_released():
			spawn_tooltip()
			

func _code_edit() -> CodeEdit:
	return EditorInterface\
			.get_script_editor().get_current_editor()\
			.find_children("", "CodeEdit", true, false)\
			.front()

func _input(event):
	#if event is InputEventMouseButton:
		#if event.button_index == MOUSE_BUTTON_LEFT and event.is_released():
			#spawn_tooltip()
	if event is InputEventKey:
		if(not EditorInterface.get_script_editor().get_current_editor()):
			refactor_tooltip.visible = false
			return
		var code_edit: CodeEdit = _code_edit()
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
			toggle_export(selected_line)
		elif event.pressed and event.ctrl_pressed and event.keycode == KEY_T:
			toggle_tool_button(selected_line)
		elif event.pressed and event.keycode == KEY_CTRL:# and not event.is_echo():
			spawn_tooltip()


func extract_function():
	var code_edit = _code_edit()
	var statements = code_edit.get_selected_text()
	var previous_column = code_edit.get_caret_column()
	var previous_line = code_edit.get_caret_line()
	mi_parser.extract_function(code_edit.text, statements, "fOO")
	code_edit.grab_focus()
	code_edit.set_caret_column(previous_column)
	code_edit.set_caret_line(previous_line)


func spawn_tooltip():
	var code_edit = _code_edit()
	if(is_instance_valid(refactor_tooltip)):
		refactor_tooltip.queue_free()
	refactor_tooltip = REFACTOR_TOOLTIP.instantiate()
	refactor_tooltip.refactorings = self
	code_edit.add_child(refactor_tooltip)
	refactor_tooltip.appear(code_edit)

func toggle_export(line_number: int):
	var code_edit = _code_edit()
	var selected_line_text = code_edit.get_line(line_number)
	var new_text = mi_parser.toggle_export(selected_line_text)
	if new_text != selected_line_text:
		var diff_columns = new_text.length() - selected_line_text.length()
		var previous_caret_column = code_edit.get_caret_column()
		code_edit.remove_line_at(line_number)
		code_edit.insert_line_at(line_number, new_text)
		code_edit.set_caret_column(max(0, previous_caret_column + diff_columns))
		code_edit.set_caret_line(line_number)


func toggle_tool_button(selected_line: int):
	var code_edit = _code_edit()
	var all_text = code_edit.text
	var selected_function = mi_parser.function_at_line(selected_line, all_text)
	var new_text = mi_parser.toggle_tool_button(selected_function, all_text)
	if new_text != all_text:
		var diff_lines = new_text.split("\n").size() - all_text.split("\n").size()
		var previous_caret_line = code_edit.get_caret_line()
		var previous_caret_column = code_edit.get_caret_column()
		code_edit.clear()
		code_edit.insert_text(new_text, 0, 0)
		#code_edit.text = new_text
		code_edit.set_caret_column(previous_caret_column)
		code_edit.set_caret_line(max(0, previous_caret_line + diff_lines))
		#return
		#var script: Script = EditorInterface.get_script_editor().get_current_script()
		#script.source_code = code_edit.text
		#ResourceSaver.save(script)
		#EditorInterface.get_selection().get_selected_nodes().map(func(node: Node):
			#node.notify_property_list_changed()
		#)

func _exit_tree():
	if(is_instance_valid(refactor_tooltip)):
		refactor_tooltip.queue_free()
