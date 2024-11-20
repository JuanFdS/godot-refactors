@tool
extends EditorPlugin

const GODOTITO = preload("res://addons/godotito/Godotito.tscn")
var godotito

func _enter_tree() -> void:
	godotito = GODOTITO.instantiate()
	EditorInterface.get_base_control().add_child(godotito, true)

func _exit_tree() -> void:
	if(godotito):
		godotito.queue_free()
