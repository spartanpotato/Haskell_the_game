# Nombre del ejecutable final
TARGET = main

# Archivos fuente de Haskell
SOURCES = $(wildcard *.hs)

# Compilar el proyecto
all: $(TARGET)

$(TARGET): $(SOURCES)
	ghc -o $(TARGET) $(SOURCES)

# Limpiar archivos generados
clean:
	rm -f $(TARGET) *.hi *.o

# Limpiar todo, incluyendo el ejecutable
dist-clean: clean
	rm -f $(TARGET)