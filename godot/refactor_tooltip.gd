@tool
extends HBoxContainer

var text: String = ""

func _process(_delta):
	$Texto.text = text
