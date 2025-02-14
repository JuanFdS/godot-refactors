# Refactorings

⚠️ **Experimental** ⚠️
Esto está _usable_ (ponele) pero super incompleto aún.

Este proyecto es un addon para agregar refactors automáticos para el editor de código de Godot.

Tiene refactors que llegan hasta Godot 4.4.

# Requerimientos para desarrollar en este repo

### Godot

https://godot-rust.github.io/book/intro/setup.html#godot-engine

### Rust

https://godot-rust.github.io/book/intro/setup.html#rust

# Como llevar cambios del proyecto en rust al proyecto de Godot

Dentro de la carpeta `rust`, correr `cargo build`. Esto debería generar archivos dentro de `godot/addons/refactorings/bin`.

# Algunos refactorings en progreso

- Extraer variable
  
  ![godot-extract-variable](https://github.com/user-attachments/assets/1c4d7d95-2d15-43c5-bc23-2393f455f3e5)

- Inlinear variable
  
  ![godot-inline-variable](https://github.com/user-attachments/assets/29b3c4b6-d0cf-4272-adca-4a2adcb67bb6)

- Alternar anotación `@export`
  
  ![godot-toggle-export](https://github.com/user-attachments/assets/af0604ff-939c-4fbd-94ca-cf4721d5d4c4)

- Activar/Desactivar la exposición de una función con la anotación `@export_tool_button` y una variable
  
  ![godot-toggle-tool-button](https://github.com/user-attachments/assets/d3929586-8b2a-49f1-a720-84be2dd4546b)

# ¿Qué falta?

Una buena forma de encontrar que cosas todavía no están implementadas, es ir a algún script valido de godot, y si la pelotita que indica si el archivo pudo ser parseado es roja, clickearla y ver todas las lineas que aparecen en rojo. Esas lineas tienen cosas que nuestro parser AUN no soporta, ¡podes implementarlo y mandar un PR!

![alt text](images/image.png)

https://github.com/JuanFdS/godot-refactors/issues
