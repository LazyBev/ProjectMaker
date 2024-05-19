#include <iostream>
#include <fstream>
#include <string>
#include <cctype>
#include <filesystem>

void Project(const std::string& fileName) {
    std::string dir = fileName;
    std::filesystem::create_directory(dir);

    std::string CcPath = dir + "/" + fileName + ".cc";
    std::string HhPath = dir + "/" + fileName + ".hh";

    std::ofstream newCc(CcPath);
    if (!newCc.is_open()) {
        std::cerr << "Failed to open " << CcPath << std::endl;
        return;
    }
    newCc << "#include <iostream>\n\nint main() {\n\treturn 0;\n}";
    newCc.close();
    
    std::string Hname = fileName;
    for (char &c : Hname) {
        c = std::toupper(static_cast<unsigned char>(c)); 
    }

    std::ofstream newHh(HhPath);
    if (!newHh.is_open()) {
        std::cerr << "Failed to open " << HhPath << ".hh" << std::endl;
        return;
    }
    newHh << "#ifndef " << Hname << "_HH\n#define " << Hname << "_HH\n\n#endif";
    newHh.close();
}

int main() {
    std::string name;
    std::cout << "Enter project name : ";
    std::cin >> name;

    Project(name);

    std::cout << "Files " << name << ".cc and " << name << ".hh created successfully." << std::endl;

    return 0;
}
