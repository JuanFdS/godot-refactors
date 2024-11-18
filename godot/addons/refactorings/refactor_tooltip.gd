@tool
extends HBoxContainer

var code_edit: CodeEdit
@onready var texto = %Texto
var offset: Vector2
var line_column: Vector2
var refactorings: Refactorings
var disappearing: bool = false

func line_number() -> int:
	return line_column.y

func column_number() -> int:
	return line_column.x

func _ready():
	%ToggleExport.pressed.connect(func():
		queue_free()
		refactorings.toggle_export(line_number())
	)
	%ToggleToolButton.pressed.connect(func():
		queue_free()
		refactorings.toggle_tool_button(line_number())
	)
	%ExtractVariable.pressed.connect(func():
		queue_free()
		refactorings.extract_variable()
	)


func appear(a_code_edit: CodeEdit):
	visible = true
	code_edit = a_code_edit
	code_edit.text_changed.connect(func():
		queue_free(),
		CONNECT_ONE_SHOT
	)
	texto.text = code_edit.get_word_at_pos(code_edit.get_local_mouse_pos())
	offset = Vector2.LEFT * size.x / 2.0
	var local_mouse_pos = code_edit.get_local_mouse_pos()
	line_column = code_edit.get_line_column_at_pos(code_edit.get_local_mouse_pos(), true)


func _process(_delta):
	if EditorInterface.get_edited_scene_root() == self or is_queued_for_deletion():
		return
	if(not code_edit or not code_edit.is_visible_in_tree()):
		visible = false
	else:
		var pos = code_edit.get_pos_at_line_column(line_column.y, line_column.x)
		position = Vector2(pos) + offset
		if(abs(global_position.y - get_global_mouse_position().y) > 50.0):
			visible = false
		if(abs(global_position.x - get_global_mouse_position().x) > 300.0):
			visible = false
