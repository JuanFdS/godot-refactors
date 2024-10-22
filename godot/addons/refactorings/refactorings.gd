@tool
extends EditorPlugin

const REFACTORINGS = preload("res://refactorings.tscn")
var refactorings

func _enter_tree():
	refactorings = REFACTORINGS.instantiate()
	EditorInterface.get_base_control().add_child(refactorings)


func _exit_tree():
	if(refactorings):
		refactorings.queue_free()
