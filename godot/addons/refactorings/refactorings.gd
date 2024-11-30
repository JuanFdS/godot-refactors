@tool
class_name Refactorings extends Control

const CodeEditWithRefactors = preload("res://code_edit_with_refactors.gd")
const REFACTOR_TOOLTIP = preload("res://addons/refactorings/refactor_tooltip.tscn")

@onready var indicador = %Indicador
@onready var mi_parser: MiParser = $MiParser
var refactor_tooltip
var detected_errors = []

func _ready():
	if Engine.is_editor_hint() and get_tree().edited_scene_root == self:
		return
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
			code_edit.gui_input.connect(self.handle_input_on_code_edit.bind(code_edit))
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

func handle_input_on_code_edit(event: InputEvent, code_edit: CodeEdit):
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
			var focused_function = mi_parser.function_at_line(selected_line, all_text)
			code_edit.text = mi_parser.bajar(selected_line, all_text)
			var pos = code_edit.search(focused_function.split("\n")[0], TextEdit.SEARCH_MATCH_CASE, 0, 0)
			var amount_of_lines = focused_function.split("\n").size()
			var finishing_caret_line = pos.y + amount_of_lines - 1
			var finishing_caret_column = code_edit.get_line_width(finishing_caret_line)
			code_edit.select(pos.y, pos.x, finishing_caret_line, finishing_caret_column)
		elif event.pressed and event.ctrl_pressed and event.keycode == KEY_UP:
			var focused_function = mi_parser.function_at_line(selected_line, all_text)
			code_edit.text = mi_parser.subir(selected_line, all_text)
			var pos = code_edit.search(focused_function.split("\n")[0], TextEdit.SEARCH_MATCH_CASE, 0, 0)
			var amount_of_lines = focused_function.split("\n").size()
			var finishing_caret_line = pos.y + amount_of_lines - 1
			var finishing_caret_column = code_edit.get_line_width(finishing_caret_line)
			code_edit.select(pos.y, pos.x, finishing_caret_line, finishing_caret_column)
		elif event.pressed and event.ctrl_pressed and event.keycode == KEY_E:
			toggle_export(selected_line)
		elif event.pressed and event.ctrl_pressed and event.keycode == KEY_T:
			toggle_tool_button(selected_line)
		elif event.pressed and event.keycode == KEY_CTRL:# and not event.is_echo():
			spawn_tooltip()


func extract_variable():
	var code_edit = _code_edit()
	var start_column = code_edit.get_selection_origin_column()
	var start_line = code_edit.get_selection_origin_line()
	var end_column = code_edit.get_selection_to_column()
	var end_line = code_edit.get_selection_to_line()
	var new_text_and_selection = mi_parser.extract_variable(
		code_edit.text, start_line, start_column, end_line, end_column, "asd"
	)
	var new_text = new_text_and_selection[0]
	var selection = new_text_and_selection[1]
	code_edit.text = new_text
	select_ranges(selection)

func _evaluation_result_as_text(code_to_evaluate: String):
	var expression := Expression.new()

	var error = expression.parse(code_to_evaluate)
	if error != OK:
		print(expression.get_error_text())
		return

	var selected_nodes = EditorInterface.get_selection().get_selected_nodes()
	var current_script = EditorInterface.get_script_editor().get_current_script()
	var context = null
	if not selected_nodes.is_empty() and selected_nodes.front().get_script() == current_script:
			context = selected_nodes.front()

	var result = expression.execute([], context)

	if expression.has_execute_failed():
		print(expression.get_error_text())
		return

	var new_text: String
	if result is String:
		new_text = "\"%s\"" % result
	else:
		new_text = str(result)
	return new_text

func evaluate_in_place():
	var code_edit = _code_edit()
	var text = code_edit.get_selected_text()
	var result = _evaluation_result_as_text(text)
	if result:
		code_edit.insert_text_at_caret(result)

func evaluate_and_print():
	var code_edit = _code_edit()
	var text = code_edit.get_selected_text()
	var result = _evaluation_result_as_text(text)
	if not result:
		return

	var evaluation_prefix: String = " #=> "
	var line = code_edit.get_selection_to_line()
	var line_previous_text: String = code_edit.get_line(line)
	var line_column_end = line_previous_text.length()
	var column: int
	if evaluation_prefix in line_previous_text:
		column = line_previous_text.find(evaluation_prefix)
		code_edit.remove_text(
			line, column,
			line, line_column_end
		)
	else:
		column = line_column_end
	var new_evaluation: String = "%s %s" % [evaluation_prefix, result]
	code_edit.insert_text(new_evaluation, line, column)
	code_edit.grab_focus()

func inline_variable():
	var code_edit = _code_edit()
	var previous_column = code_edit.get_caret_column()
	var previous_line = code_edit.get_caret_line()
	var start_column = code_edit.get_selection_origin_column()
	var start_line = code_edit.get_selection_origin_line()
	var end_column = code_edit.get_selection_to_column()
	var end_line = code_edit.get_selection_to_line()
	var new_text_and_selection = mi_parser.inline_variable(
		code_edit.text, start_line, start_column, end_line, end_column 
	)
	var new_text = new_text_and_selection[0]
	var selection = new_text_and_selection[1]
	code_edit.text = new_text
	code_edit.select(previous_line, previous_column, previous_line, previous_column, 0)
	code_edit.grab_focus()

func select_ranges(ranges):
	var code_edit: CodeEdit = _code_edit()
	var caret_idx: int = 0
	for range in ranges:
		var start_pos: Vector2i = range[0]
		var end_pos: Vector2i = range[1]
		if caret_idx >= code_edit.get_caret_count():
			var caret_index = code_edit.add_caret(start_pos.x, start_pos.y)
		code_edit.deselect(caret_idx)
		code_edit.select(start_pos.x, start_pos.y, end_pos.x, end_pos.y, caret_idx)
		caret_idx += 1
	code_edit.grab_focus()

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
	var new_text = mi_parser.toggle_tool_button(selected_line, all_text)
	if new_text != all_text:
		var diff_lines = new_text.split("\n").size() - all_text.split("\n").size()
		var previous_caret_line = code_edit.get_caret_line()
		var previous_caret_column = code_edit.get_caret_column()
		code_edit.begin_complex_operation()
		code_edit.clear()
		code_edit.insert_text(new_text, 0, 0)
		#code_edit.text = new_text
		code_edit.set_caret_column(previous_caret_column)
		code_edit.set_caret_line(max(0, previous_caret_line + diff_lines))
		code_edit.end_complex_operation()
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
