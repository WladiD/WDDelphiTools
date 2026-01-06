# Generics Study: Performance Benchmark

This study compares different approaches to using Generics in Delphi, focusing on compilation performance and binary size overhead when dealing with a large number of specialized collection types.

## Intention

The goal is to evaluate the scalability of different Generics implementations when a project requires a massive amount of distinct list and dictionary types.

The benchmark generates:
- **500 distinct Record types** (defined across 50 units).
- **500 specialized List types** (one for each record type).
- **1,500 specialized Dictionary types** (three for each record type: String-keyed, Integer-keyed, and GUID-keyed).

In total, over **2,000 distinct collection specializations** are compiled and instantiated in each implementation project.

## Implementations Compared

1.  **StaticGenerics (TmplCodeGen):**
    - Uses a template-based code generator (`TmplCodeGen`) to pre-generate pure Pascal source code for each specialized type (e.g., `TList_Integer`).
    - The compiler sees standard, non-generic Pascal code.
2.  **mORMotGenerics:**
    - Uses the lightweight generics implementation from the mORMot 2 framework (`mormot.core.collections`).
    - Known for fast compilation and low overhead.
3.  **DelphiGenerics:**
    - Uses the standard Delphi RTL generics (`System.Generics.Collections`).
    - Heavily reliant on the compiler's generic expansion mechanism.
4.  **SpringGenerics:**
    - Uses the Spring4D framework (`Spring.Collections`).
    - Offers a very rich and feature-complete collections library with extensive interface usage.

## Test Environment

- **CPU:** Intel i7-12800HX
- **SSD:** NVMe Micron 512 GB
- **RAM:** 32 GB
- **OS:** Windows 11
- **Compiler:** Delphi 12 Athens
- **mORMot 2:** Commit `4428ef84989200175a27ef90106e22b6caba116e`
- **Spring4D:** Commit `ad9dbde20a8c2409a95c7dff443d4d16520d971f`

## Results

*Benchmark run on 2026-01-16*

| Implementation               | Compile Time | Lines Compiled | Binary Code Size    | Max Memory used by dcc32 | Status     |
| :---                         | :---         | :---           | :---                | :---                     | :---       |
| **StaticGenerics**           | **2.94 s**   | 549,511        | 9,133,236 Bytes     | **164 MB**               | Success    |
| **mORMotGenerics**           | 4.11 s       | 157,469        | **7,782,624 Bytes** | 236 MB                   | Success    |
| **DelphiGenerics**           | 88.44 s      | 12,471         | 29,794,292 Bytes    | 920 MB                   | Success    |
| **SpringGenerics (Full)**    | > 23 min     | -              | -                   | > 3,800 MB               | **FAILED** |
| **SpringGenerics (Reduced)** | 301.59 s     | 228,764        | 31,312,656 Bytes    | 2,036 MB                 | Success    |

### Analysis

*   **StaticGenerics** demonstrates the fastest compilation time and the lowest memory footprint. By pre-generating the code, the workload is shifted from the Delphi compiler to the code generator. The compiler handles over half a million lines of plain Pascal code in under 3 seconds using only 164 MB of RAM.
*   **mORMotGenerics** is incredibly efficient, nearly matching the speed of static code and producing the smallest binary. Its memory usage is also very low (236 MB).
*   **DelphiGenerics** shows significant scalability issues. Compilation takes nearly 1.5 minutes (30x slower than static), and the binary size balloons to nearly 30 MB. 
*   **SpringGenerics (Full = Conditional FULL_TEST defined)** hit a hard limit. The compiler crashed with an **"Out of Memory" (F2046)** error after running for **23 minutes**, having consumed over **> 3,800 MB** of RAM. This indicates that the complex, interface-heavy generic structures of Spring4D put an enormous strain on the compiler's internal heap and symbol management.
*   **SpringGenerics (Reduced = Conditional FULL_TEST not defined)**: Even with only **50% of the load** (1,000 instantiations), the compiler takes over 5 minutes and consumes ~2 GB of RAM. The binary size is comparable to standard Delphi Generics.

## Conclusion

For projects requiring a massive number of specialized collection types:
- **StaticGenerics (Code Generation)** and **mORMotGenerics** are the only viable options for maintaining fast iteration cycles and keeping binary sizes low.
- Standard **Delphi Generics** quickly become a bottleneck in both build time and executable size.
- **Spring4D**, despite its features, is not suitable for this specific high-volume scalability scenario due to compiler limitations.
