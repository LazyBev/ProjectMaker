#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <ctype.h>
#include <strings.h>

#define MAX_PATH 4096
#define MAX_CONTENT 2048

typedef enum { Ok, Err } Result;

typedef struct {
    const char *name;
    const char *ext;
    const char *main_template;
    const char *header_ext;
    const char *header_template;
    const char *makefile;
} Lang;

Result write_file(const char *path, const char *contents) {
    FILE *f = fopen(path, "w");
    if (!f) { perror("fopen"); return Err; }
    fputs(contents, f);
    fclose(f);
    return Ok;
}

void upper(char *s) {
    for (; *s; s++) *s = toupper((unsigned char)*s);
}

void str_replace(char *s, const char *from, const char *to) {
    char buf[MAX_CONTENT];
    char *p;
    size_t fl = strlen(from);
    while ((p = strstr(s, from))) {
        size_t pos = p - s;
        strncpy(buf, s, pos);
        buf[pos] = '\0';
        strcat(buf, to);
        strcat(buf, p + fl);
        strcpy(s, buf);
    }
}

int main(int argc, char **argv) {
    char name[MAX_PATH] = "main";
    char *lang_str = NULL;

    if (argc >= 3) {
        strncpy(name, argv[1], MAX_PATH - 1);
        lang_str = argv[2];
    } else if (argc >= 2) {
        lang_str = argv[1];
    } else {
        printf("Usage: %s [project-name] <language>\n", argv[0]);
        return 1;
    }

    Lang langs[] = {
        { "C", ".c",
            "#include <stdio.h>\n#include \"{NAME}.h\"\n\nint main() {\n    printf(\"Hello, World!\\n\");\n    return 0;\n}\n",
            ".h",
            "#ifndef {GUARD}_H\n#define {GUARD}_H\n\n#endif\n",
            ".PHONY: all clean\nall:\n\tgcc -o a *.c\nclean:\n\trm -f a\n" },
        { "C++", ".cc",
            "#include <iostream>\n#include \"{NAME}.hh\"\n\nint main() {\n    std::cout << \"Hello, World!\" << std::endl;\n    return 0;\n}\n",
            ".hh",
            "#ifndef {GUARD}_HH\n#define {GUARD}_HH\n\n#endif\n",
            ".PHONY: all clean\nall:\n\tg++ -o a *.cc\nclean:\n\trm -f a\n" },
        { "Python", ".py",
            "def main():\n    print(\"Hello, World!\")\n\nif __name__ == \"__main__\":\n    main()\n",
            NULL, NULL, NULL },
        { "Rust", ".rs",
            "fn main() {\n    println!(\"Hello, World!\");\n}\n",
            NULL, NULL, NULL },
        { "Zig", ".zig",
            "const std = @import(\"std\");\n\npub fn main() !void {\n    const stdout = std.io.getStdOut().writer();\n    try stdout.print(\"Hello, world!\");\n}\n",
            NULL, NULL, NULL },
        { "Go", ".go",
            "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello, World!\")\n}\n",
            NULL, NULL, NULL },
        { "Java", ".java",
            "public class {NAME} {\n    public static void main(String[] args) {\n        System.out.println(\"Hello, World!\");\n    }\n}\n",
            NULL, NULL, NULL },
        { "Lua", ".lua",
            "local function main()\n    print(\"Hello, World!\")\nend\n\nmain()\n",
            NULL, NULL, NULL },
        { "JavaScript", ".js",
            "function main() {\n    console.log(\"Hello, World!\");\n}\n\nmain();\n",
            NULL, NULL, NULL },
        { "TypeScript", ".ts",
            "function main(): void {\n    console.log(\"Hello, World!\");\n}\n\nmain();\n",
            NULL, NULL, NULL },
        { "C#", ".cs",
            "using System;\n\nnamespace {NAME} {\n    class Program {\n        static void Main(string[] args) {\n            Console.WriteLine(\"Hello, World!\");\n        }\n    }\n}\n",
            NULL, NULL, NULL },
        { "Kotlin", ".kt",
            "fun main() {\n    println(\"Hello, World!\")\n}\n",
            NULL, NULL, NULL },
        { "Pascal", ".pas",
            "program {NAME};\n\nbegin\n    WriteLn('Hello, World!');\nend.\n",
            NULL, NULL, NULL },
        { "Nim", ".nim",
            "proc main() =\n    echo \"Hello, World!\"\n\nwhen isMainModule:\n    main()\n",
            NULL, NULL, NULL },
        { "Haskell", ".hs",
            "module Main where\n\nmain :: IO ()\nmain = putStrLn \"Hello, World\"\n",
            NULL, NULL, NULL },
        { "Ruby", ".rb",
            "def main\n    puts \"Hello, World!\"\nend\n\nmain\n",
            NULL, NULL, NULL },
    };

    int n_langs = sizeof(langs) / sizeof(langs[0]);
    Lang *match = NULL;
    for (int i = 0; i < n_langs; i++) {
        if (strcasecmp(lang_str, langs[i].name) == 0) {
            match = &langs[i];
            break;
        }
    }
    if (!match) {
        fprintf(stderr, "Unsupported language: %s\n", lang_str);
        return 1;
    }

    if (mkdir(name, 0755) != 0) { perror("mkdir"); return 1; }

    char main_path[MAX_PATH], guard[256];
    snprintf(main_path, sizeof(main_path), "%s/%s%s", name, name, match->ext);
    snprintf(guard, sizeof(guard), "%s_%s_H", name, name);
    upper(guard);

    char mc[MAX_CONTENT];
    snprintf(mc, sizeof(mc), "%s", match->main_template);
    str_replace(mc, "{NAME}", name);
    str_replace(mc, "{GUARD}", guard);
    if (write_file(main_path, mc) == Err) return 1;

    if (match->header_template) {
        char header_path[MAX_PATH], hc[MAX_CONTENT];
        snprintf(header_path, sizeof(header_path), "%s/%s%s", name, name, match->header_ext);
        snprintf(hc, sizeof(hc), "%s", match->header_template);
        str_replace(hc, "{NAME}", name);
        str_replace(hc, "{GUARD}", guard);
        if (write_file(header_path, hc) == Err) return 1;
    }

    if (match->makefile) {
        char make_path[MAX_PATH];
        snprintf(make_path, sizeof(make_path), "%s/Makefile", name);
        if (write_file(make_path, match->makefile) == Err) return 1;
    }

    printf("Created %s project '%s'\n", match->name, name);
    return 0;
}
