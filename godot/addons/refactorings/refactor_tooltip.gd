@tool
extends HBoxContainer

var code_edit: CodeEdit
@onready var texto = %Texto
var offset: Vector2

func appear(a_code_edit: CodeEdit):
	visible = true
	code_edit = a_code_edit
	texto.text = code_edit.get_word_at_pos(code_edit.get_local_mouse_pos())
	offset = Vector2.DOWN * 10.0 + Vector2.LEFT * size.x / 2.0
	global_position = get_global_mouse_position() + offset

func _process(_delta):
	if EditorInterface.get_edited_scene_root() == self:
		return
	if(not code_edit or not code_edit.is_visible_in_tree()):
		visible = false
	if((global_position - offset).distance_to(get_global_mouse_position()) > 150.0):
		visible = false

		
		
