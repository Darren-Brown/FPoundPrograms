module DataTypes

    type cellState =
        struct

            val content:char
            val visible:bool
            new(cont, vis) = {content = cont; visible = vis}
        end

