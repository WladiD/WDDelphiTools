# StaticGenerics for Delphi

**StaticGenerics** is a project aimed at generating fully typed, static collection classes (Lists and Dictionaries) for Delphi at compile-time. It leverages code generation to produce concrete implementations for specific types, offering a powerful alternative to Delphi's runtime generics (`TList<T>`, `TDictionary<K,V>`).

## 🚀 Why StaticGenerics?

### ⚡ Drastic Compile-Time Improvements
One of the primary motivations for this project is **performance during the build process**.
*   **Faster Compilation & Linking:** Heavily using standard generics in large Delphi projects can significantly increase compilation and linking times due to the way the compiler instantiates generics for each type. Static classes are pre-generated source code, allowing the compiler to process them much faster, just like regular code.
*   **Reduced Binary Size (Code Bloat):** While Delphi's linker is smart, excessive use of generics can sometimes lead to "code bloat". Static generation gives you precise control over what gets generated.

### 🛠️ Strong Typing & Interoperability
*   **Clear Interfaces:** Instead of generic signatures like `IList<Integer>`, you work with explicit interfaces like `IList_Integer`. This is often cleaner and safer, especially when exposing interfaces across package or DLL boundaries.
*   **Tailored Implementations:** The templates allow for specific optimizations based on the type (e.g., special handling for Strings or Interfaces) that generic implementations might handle less efficiently.

## ⚙️ How It Works

The system uses a custom tool, `TmplCodeGen`, to generate Pascal code based on templates and JSON configuration files.

1.  **Configuration (`*.json`):**
    You define the types you need collections for.
    *   `System.List-conf.json`: Defines list types (e.g., `Integer`, `String`, `TGUID`).
    *   `System.Dictionary-conf.json`: Defines dictionary key-value pairs.

2.  **Templates (`Templates\*.TMPL.pas`):**
    Mustache templates define the structure of the Pascal code for Lists and Dictionaries.

3.  **Generation Process:**
    *   `_RunTmplCodeGen.bat` executes the generator.
    *   It creates temporary Pascal fragments based on the configurations.
    *   It **injects** these fragments directly into the source files (`System.Collections.Interfaces.pas` and `System.Collections.Factory.pas`) into designated `{$REGION}` blocks.

## 📂 Project Structure

*   **`Source/`**: Contains the core library files where the generated code is injected.
    *   `System.Collections.Interfaces.pas`: Definitions of interfaces (e.g., `IList_Integer`).
    *   `System.Collections.Factory.pas`: Implementation logic and factory methods.
*   **`Templates/`**: The Pascal code templates used for generation.
*   **`*-conf.json`**: Configuration files defining which types to generate.
*   **`_RunTmplCodeGen.bat`**: The script to trigger the code generation.

## 📦 Usage

To regenerate the collections after modifying a JSON config or a template:

1.  Run `_RunTmplCodeGen.bat` in the root of this folder.
2.  The files in `Source/` will be updated with the new definitions.

## 📝 License

Licensed under the MIT License.
