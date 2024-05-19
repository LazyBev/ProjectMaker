#include <iostream>
#include <fstream>
#include <string>
#include <cctype>
#include <filesystem>

void Project(const std::string& fileName, const std::string& fileLang) {
    std::string dir = fileName;
    std::string MPath = "";
    std::string SPath = "";
    std::string MakePath = "";

    std::filesystem::create_directory(dir);

    if(fileLang == "C++" || fileLang == "Cpp") { 
        MPath = dir + "/" + fileName + ".cc";
        SPath = dir + "/" + fileName + ".hh";
        MakePath = dir + "/" + "Makefile";

        std::ofstream newMake(MakePath);
        if (!newMake.is_open()) {
            std::cerr << "Failed to open " << MakePath << std::endl;
            return;
        }
        newMake << ".PHONY: all\n\nall:\n\tg++ *cc";
        newMake.close();

        std::ofstream newM(MPath);
        if (!newM.is_open()) {
            std::cerr << "Failed to open " << MPath << std::endl;
            return;
        }
        newM << "#include <iostream>\n\nint main() {\n\tstd::cout << \"Hello, World!\" << std::endl;\n\n\treturn 0;\n}";
        newM.close();
        
        std::string Sname = fileName;
        for (char &c : Sname) {
            c = std::toupper(static_cast<unsigned char>(c)); 
        }

        std::ofstream newS(SPath);
        if (!newS.is_open()) {
            std::cerr << "Failed to open " << SPath << std::endl;
            return;
        }
        newS << "#ifndef " << Sname << "_HH\n#define " << Sname << "_HH\n\n#endif";
        newS.close();
    } else if(fileLang == "C") {
        MPath = dir + "/" + fileName + ".c";
        SPath = dir + "/" + fileName + ".h";
        MakePath = dir + "/" + "Makefile";

        std::ofstream newMake(MakePath);
        if (!newMake.is_open()) {
            std::cerr << "Failed to open " << MakePath << std::endl;
            return;
        }
        newMake << ".PHONY: all\n\nall:\n\tgcc *c";
        newMake.close();

        std::ofstream newM(MPath);
        if (!newM.is_open()) {
            std::cerr << "Failed to open " << MPath << std::endl;
            return;
        }
        newM << "#include <stdio.h>\n\nint main() {\n\tprintf(\"Hello, World!\");\n\n\treturn 0;\n}";
        newM.close();
        
        std::string Sname = fileName;
        for (char &c : Sname) {
            c = std::toupper(static_cast<unsigned char>(c)); 
        }

        std::ofstream newS(SPath);
        if (!newS.is_open()) {
            std::cerr << "Failed to open " << SPath << std::endl;
            return;
        }
        newS << "#ifndef " << Sname << "_H\n#define " << Sname << "_H\n\n#endif";
        newS.close();
    } else if(fileLang == "Python" || fileLang == "Py") {
        MPath = dir + "/" + fileName + ".py";

        std::ofstream newM(MPath);
        if (!newM.is_open()) {
            std::cerr << "Failed to open " << MPath << std::endl;
            return;
        }
        newM << "def main():\n\tprint(\"Hello, World!\")";
        newM.close();
    } else if(fileLang == "Pascal" || fileLang == "Pas") {
        MPath = dir + "/" + fileName + ".pas";

        std::ofstream newM(MPath);
        if (!newM.is_open()) {
            std::cerr << "Failed to open " << MPath << std::endl;
            return;
        }
        newM << "program "+ fileLang +";\n\nbegin\n\tWriteLn('Hello, World!');\nend.";
        newM.close();
    }
}

int main() {
    std::string name;
    std::cout << "Enter projects name: ";
    std::cin >> name;

    std::string lang;
    std::cout << "Enter projects language (Capital Letter): ";
    std::cin >> lang;

    Project(name, lang);

    std::cout << "Project '" << name << "' ("+ lang +") was successfully made" << std::endl;

    return 0;
}
