#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/stat.h>
#include <ctype.h>

typedef enum {
    Ok,
    Err,
} Result;

Result writeToFile(const char *path, const char *contents);
Result project(const char *projectName, const char *projectLanguage);
void toUpperCase(char *str);

int main(int argc, char* argv[]) {
    char name[100];
    char lang[100];

    if (argc >= 2 && strcmp(argv[1], "-n") == 0) {
        printf("Enter project's name: ");
        scanf("%99s", name);
    } else {
        strcpy(name, "main");
    }

    printf("Enter project's language (Capital Letter): ");
    scanf("%99s", lang);

    system("clear");

    if (project(name, lang) != Ok) return 1;

    printf("Project '%s' (%s) was successfully made\n", name, lang);
    return 0;
}

Result writeToFile(const char *path, const char *contents) {
    FILE *file = fopen(path, "w");
    if (file == NULL) {
        fprintf(stderr, "Failed to open %s\n", path);
        return Err;
    }
    fprintf(file, "%s\n", contents);
    fclose(file);
    return Ok;
}

void toUpperCase(char *str) {
    for (; *str; ++str) {
        *str = toupper((unsigned char) *str);
    }
}

Result project(const char *projectName, const char *projectLanguage) {
    if (mkdir(projectName, 0755) == -1) {
        perror("Failed to create directory");
        return Err;
    }

    char mainPath[256], headerPath[256], makePath[256], headerGuard[256];
    snprintf(mainPath, sizeof(mainPath), "%s/%s", projectName, projectName);
    snprintf(headerPath, sizeof(headerPath), "%s/%s.h", projectName, projectName);
    snprintf(makePath, sizeof(makePath), "%s/Makefile", projectName);
    snprintf(headerGuard, sizeof(headerGuard), "%s_%s_H", projectName, projectName);
    toUpperCase(headerGuard);

    if (strcmp(projectLanguage, "C++") == 0 || strcmp(projectLanguage, "Cpp") == 0) {
        strcat(mainPath, ".cc");
        snprintf(headerPath, sizeof(headerPath), "%s/%s.hh", projectName, projectName);
        if (writeToFile(makePath, ".PHONY: all\n\nall:\n\tg++ -o a *.cc") == Err) return Err;
        char mainContent[1024];
        snprintf(mainContent, sizeof(mainContent),
            "#include <iostream>\n"
            "#include \"%s.hh\"\n\n"
            "int main() {\n"
            "\tstd::cout << \"Hello, World!\" << std::endl;\n\n"
            "\treturn 0;\n"
            "}", projectName);
        if (writeToFile(mainPath, mainContent) == Err) return Err;
        char headerContent[1024];
        snprintf(headerContent, sizeof(headerContent),
            "#ifndef %s_HH\n"
            "#define %s_HH\n\n"
            "#endif", headerGuard, headerGuard);
        if (writeToFile(headerPath, headerContent) == Err) return Err;
    } else if (strcmp(projectLanguage, "C") == 0) {
        strcat(mainPath, ".c");
        if (writeToFile(makePath, ".PHONY: all\n\nall:\n\tgcc -o a *.c") == Err) return Err;
        char mainContent[1024];
        snprintf(mainContent, sizeof(mainContent),
            "#include <stdio.h>\n"
            "#include \"%s.h\"\n\n"
            "int main() {\n"
            "\tprintf(\"Hello, World!\\n\");\n\n"
            "\treturn 0;\n"
            "}", projectName);
        if (writeToFile(mainPath, mainContent) == Err) return Err;
        char headerContent[1024];
        snprintf(headerContent, sizeof(headerContent),
            "#ifndef %s_H\n"
            "#define %s_H\n\n"
            "#endif", headerGuard, headerGuard);
        if (writeToFile(headerPath, headerContent) == Err) return Err;
    } else if (strcmp(projectLanguage, "Python") == 0 || strcmp(projectLanguage, "Py") == 0) {
        strcat(mainPath, ".py");
        if (writeToFile(mainPath, "def main():\n\tprint(\"Hello, World!\")") == Err) return Err;
    } else if (strcmp(projectLanguage, "Pascal") == 0 || strcmp(projectLanguage, "Pas") == 0) {
        strcat(mainPath, ".pas");
        if (writeToFile(mainPath,
            "program main;\n\n"
            "begin\n"
            "\tWriteLn('Hello, World!');\n"
            "end."
        ) == Err) return Err;
    } else if (strcmp(projectLanguage, "Lua") == 0) {
        strcat(mainPath, ".lua");
        if (writeToFile(mainPath,
            "local function main()\n"
            "\tprint(\"Hello, World!\")\n"
            "end\n\n"
            "main()"
        ) == Err) return Err;
    } else if (strcmp(projectLanguage, "Javascript") == 0 || strcmp(projectLanguage, "JavaScript") == 0) {
        strcat(mainPath, ".js");
        if (writeToFile(mainPath,
            "function main() {\n"
            "\tconsole.log('Hello, World!');\n"
            "}\n\n"
            "main();"
        ) == Err) return Err;
    } else if (strcmp(projectLanguage, "Java") == 0) {
        strcat(mainPath, ".java");
        char mainContent[1024];
        snprintf(mainContent, sizeof(mainContent),
            "public class %s {\n"
            "\tpublic static void main(String[] args) {\n"
            "\t\tSystem.out.println(\"Hello, World!\");\n"
            "\t}\n"
            "}", projectName);
        if (writeToFile(mainPath, mainContent) == Err) return Err;
    } else if (strcmp(projectLanguage, "C#") == 0 || strcmp(projectLanguage, "Cs") == 0) {
        strcat(mainPath, ".cs");
        char mainContent[1024];
        snprintf(mainContent, sizeof(mainContent),
            "using System;\n\n"
            "namespace %s {\n"
            "\tclass Program {\n"
            "\t\tstatic void Main(string[] args) {\n"
            "\t\t\tConsole.WriteLine(\"Hello, World!\");\n"
            "\t\t}\n"
            "\t}\n"
            "}", projectName);
        if (writeToFile(mainPath, mainContent) == Err) return Err;
    } else if (strcmp(projectLanguage, "Rust") == 0 || strcmp(projectLanguage, "Rs") == 0) {
        strcat(mainPath, ".rs");
        if (writeToFile(mainPath,
            "pub fn main() {\n"
            "\tprintln!(\"Hello, World!\");\n"
            "}"
        ) == Err) return Err;
    } else if (strcmp(projectLanguage, "Zig") == 0) {
        strcat(mainPath, ".zig");
        if (writeToFile(mainPath,
            "const std = @import(\"std\");\n\n"
            "pub fn main() !void {\n"
            "\tconst stdout = std.io.getStdOut().writer();\n"
            "\ttry stdout.print(\"Hello, world!\");\n"
            "}"
        ) == Err) return Err;
    } else if (strcmp(projectLanguage, "Nim") == 0) {
        strcat(mainPath, ".nim");
        if (writeToFile(mainPath,
            "proc main() =\n"
            "\techo \"Hello, world!\"\n\n"
            "when isMainModule:\n"
            "\tmain()"
        ) == Err) return Err;
    } else if (strcmp(projectLanguage, "Kotlin") == 0 || strcmp(projectLanguage, "Kot") == 0) {
        strcat(mainPath, ".kt");
        if (writeToFile(mainPath,
            "fun main() {\n"
            "\tprintln(\"Hello, world!\")\n"
            "}"
        ) == Err) return Err;
    } else if (strcmp(projectLanguage, "Haskell") == 0 || strcmp(projectLanguage, "Hask") == 0) {
        strcat(mainPath, ".hs");
        if (writeToFile(mainPath,
            "module Start\n"
            "\t( main\n"
            "\t) where\n\n"
            "main :: IO ()\n"
            "main = putStrLn \"Hello, World\""
        ) == Err) return Err;
    } else {
        fprintf(stderr, "Language %s not supported\n", projectLanguage);
        return Err;
    }
    return Ok;
}
