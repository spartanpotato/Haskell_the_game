# Nombre del ejecutable final
TARGET = main

# Archivos fuente 
SOURCES = $(wildcard *.hs)

# Compilar 
all: $(TARGET)

$(TARGET): $(SOURCES)
	ghc -o $(TARGET) $(SOURCES)

# Limpiar 
clean:
	rm -f $(TARGET) *.hi *.o
