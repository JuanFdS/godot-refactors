@tool
extends EditorPlugin

const REFACTORINGS = preload("res://addons/refactorings/refactorings.tscn")
var refactorings

func _enter_tree():
	refactorings = REFACTORINGS.instantiate()
	var refactorings_parent = EditorInterface.get_script_editor()\
				   .find_children("", "VBoxContainer", true, false).front()\
				   .find_children("", "HBoxContainer", true, false).front()

	refactorings_parent.add_child(refactorings)
	refactorings_parent.move_child(refactorings, 10)
				
				
	#EditorInterface.get_base_control().add_child(refactorings)


func _exit_tree():
	if(refactorings):
		refactorings.queue_free()
