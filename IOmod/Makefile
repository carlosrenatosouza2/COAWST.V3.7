FC = mpif90
AR = ar
LIB_NAME = libIOmod.a
SRC = $(wildcard src/*.f90)
OBJ = $(SRC:src/%.f90=obj/%.o)

# Regra principal: compila e gera a biblioteca
all: $(LIB_NAME)

# Criando aa biblioteca esatica a partir dos objetos
$(LIB_NAME): $(OBJ)
	$(AR) rcs $@ $^

# Compilar os arquivos fonte em objetos
obj/%.o: src/%.f90 | obj
	$(FC) -c $< -o $@ -J obj/

obj:
	mkdir -p $@

clean:
	rm -rf obj $(LIB_NAME)

.PHONY: all clean
