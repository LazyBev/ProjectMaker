#include <iostream>
#include <cstring>
#include <fstream>
#include <filesystem>

enum Result {
    Ok,
    Err,
};

std::string lang;
std::string name;

Result writeToFile(const std::string& path, const std::string& contents) {
    std::ofstream stream(path);
    if (!stream.is_open()) {
        std::cerr << "Failed to open " << path << std::endl;
        return Err;
    }
    stream << contents;
    stream.close();
    return Ok;
}

Result project(const std::string& projectName, const std::string& projectLanguage) {
    std::string dir = projectName;

    if (!std::filesystem::create_directory(dir)) {
        std::cerr << "Failed to create directory " << dir << std::endl;
        return Err;
    }

    if(projectLanguage == "C++" || projectLanguage == "Cpp") {
        std::string mainPath = dir + "/" + projectName + ".cc";
        std::string headerPath = dir + "/" + projectName + ".hh";
        std::string makePath = dir + "/" + "Makefile";
        if (writeToFile(makePath, ".PHONY: all\n\nall:\n\tg++ -o a *cc") == Err) return Err;
        if (writeToFile(mainPath,
            "#include <iostream>\n"
            "#include \"" + projectName + ".hh\"\n\n"
            "int main() {\n"
            "\tstd::cout << \"Hello, World!\" << std::endl;\n\n"
            "\treturn 0;\n"
            "}"
        ) == Err) return Err;

        std::string upperName = projectName;
        for (char &c : upperName) {
            c = std::toupper(static_cast<unsigned char>(c));
        }
        if (writeToFile(headerPath,
            "#ifndef " + upperName + "_HH\n"
            "#define " + upperName + "_HH\n\n"
            "#endif"
        ) == Err) return Err;
    } else if(projectLanguage == "C") {
        std::string mainPath = dir + "/" + projectName + ".c";
        std::string headerPath = dir + "/" + projectName + ".h";
        std::string makePath = dir + "/" + "Makefile";
        if (writeToFile(makePath, ".PHONY: all\n\nall:\n\tgcc -o a *c") == Err) return Err;
        if (writeToFile(mainPath,
            "#include <stdio.h>\n"
            "#include \"" + projectName + ".h\"\n\n"
            "int main() {\n"
            "\tprintf(\"Hello, World" + "\\" + "n" + "\");\n\n"
            "\treturn 0;\n"
            "}"
        ) == Err) return Err;

        std::string upperName = projectName;
        for (char &c : upperName) {
            c = std::toupper(static_cast<unsigned char>(c));
        }
        if (writeToFile(headerPath,
            "#ifndef " + upperName + "_H\n"
            "#define " + upperName + "_H\n\n"
            "#endif"
        ) == Err) return Err;
    } else if(projectLanguage == "Python" || projectLanguage == "Py") {
        std::string mainPath = dir + "/" + projectName + ".py";
        if (writeToFile(mainPath,
            "def main():\n\tprint(\"Hello, World!\")"
        ) == Err) return Err;
    } else if(projectLanguage == "Pascal" || projectLanguage == "Pas") {
        std::string mainPath = dir + "/" + projectName + ".pas";
        if (writeToFile(mainPath,
            "program main;\n\n"
            "begin\n"
            "\tWriteLn('Hello, World!');\n"
            "end."
        ) == Err) return Err;
    } else if(projectLanguage == "Lua") {
        std::string mainPath = dir + "/" + projectName + ".lua";
        if (writeToFile(mainPath,
            "local function main()\n"
            "\tprint(\"Hello, World!\")\n"
            "end\n\n"
            "main()"
        ) == Err) return Err;
    } else if(projectLanguage == "Javascript" || projectLanguage == "JavaScript") {
        std::string mainPath = dir + "/" + projectName + ".js";
        if (writeToFile(mainPath,
            "function main() {\n"
            "\tconsole.log('Hello, World!');\n"
            "}\n\n"
            "main();"
        ) == Err) return Err;
    } else if(projectLanguage == "Java") {
        std::string mainPath = dir + "/" + projectName + ".java";
        if (writeToFile(mainPath,
            "public class " + projectName + " {\n"
            "\tpublic static void main(String[] args) {\n"
            "\t\tSystem.out.println(\"Hello, World!\");\n"
            "\t}\n"
            "}"
        ) == Err) return Err;
    } else if(projectLanguage == "C#" || projectLanguage == "Cs") {
        std::string mainPath = dir + "/" + projectName + ".cs";
        if (writeToFile(mainPath,
            "using System;\n\n"
            "namespace " + projectName + " {\n"
            "\tclass Program {\n"
            "\t\tstatic void Main(string[] args) {\n"
            "\t\t\tConsole.WriteLine(\"Hello, World!\");\n"
            "\t\t}\n"
            "\t}\n"
            "}"
        ) == Err) return Err;
    } else if(projectLanguage == "Rust" || projectLanguage == "Rs") {
        std::string mainPath = dir + "/" + projectName + ".rs";
        if (writeToFile(mainPath,
            "pub fn main() {\n"
            "\tprintln!(\"Hello, World!\");\n"
            "}"
        ) == Err) return Err;
    } else if(projectLanguage == "Zig") {
        std::string mainPath = dir + "/" + projectName + ".zig";
        if (writeToFile(mainPath,
            "const std = @import(\"std\");\n\n"
            "pub fn main() !void {\n"
            "\tconst stdout = std.io.getStdOut().writer();\n"
            "\ttry stdout.print(\"Hello, world!\");\n"
            "}"
        ) == Err) return Err;
    } else if(projectLanguage == "Nim") {
        std::string mainPath = dir + "/" + projectName + ".nim";
        if (writeToFile(mainPath,
            "proc main() =\n"
            "\techo \"Hello, world!\"\n\n"
            "when isMainModule:\n"
            "\tmain()"
        ) == Err) return Err;
    } else if(projectLanguage == "Kotlin" || projectLanguage == "Kot") {
        std::string mainPath = dir + "/" + projectName + ".kt";
        if (writeToFile(mainPath,
            "fun main() {\n"
            "\tprintln(\"Hello, world!\")\n"
            "}"
        ) == Err) return Err;
    }  else if(projectLanguage == "Haskell" || projectLanguage == "Hask") {
        std::string mainPath = dir + "/" + projectName + ".hs";
        if (writeToFile(mainPath,
            "module Start\n"
            "\t( main\n"
            "\t) where\n\n"
            "main :: IO ()\n"
            "main = putStrLn \"Hello, World\""
        ) == Err) return Err;
    } else {
        std::cerr << "Language " << projectLanguage << " not supported" << std::endl;
        return Err;
    }
    return Ok;
}

int main(int argc, char* argv[]) {
    if(argc >= 1 && strcmp(argv[1], "-n") == 0) {
        std::cout << "Enter projects name: ";
        std::cin >> name;
    } else {
        name = "main";
    }

    std::cout << "Enter projects language (Capital Letter): ";
    std::cin >> lang;

    system("clear");    
    if (project(name, lang) != Ok) return 1;

    std::cout << "Project '" << name << "' (" << lang << ") was successfully made" << std::endl;
    return 0;
}
