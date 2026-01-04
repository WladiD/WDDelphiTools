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

## Results

*Benchmark run on 2026-01-04 (Delphi 12 Athens)*

| Implementation | Compile Time | Lines Compiled | Binary Code Size | Status |
| :--- | :--- | :--- | :--- | :--- |
| **StaticGenerics** | **2.94 s** | 549,511 | 9,133,236 Bytes | Success |
| **mORMotGenerics** | **4.11 s** | 157,469 | 7,782,624 Bytes | Success |
| **DelphiGenerics** | **88.44 s** | 12,471 | 29,794,292 Bytes | Success |
| **SpringGenerics** | > 23 min | - | - | **FAILED** |

### Analysis

*   **StaticGenerics** demonstrates the fastest compilation time. By pre-generating the code, the workload is shifted from the Delphi compiler to the code generator. The compiler handles over half a million lines of plain Pascal code in under 3 seconds. The binary size is very compact (~9 MB).
*   **mORMotGenerics** is incredibly efficient, nearly matching the speed of static code and producing the smallest binary (~7.8 MB). It proves to be a highly optimized alternative for standard tasks.
*   **DelphiGenerics** shows significant scalability issues. Compilation takes nearly 1.5 minutes (30x slower than static), and the binary size balloons to nearly 30 MB. This suggests considerable overhead in the compiler's handling of many generic instantiations.
*   **SpringGenerics** hit a hard limit. The compiler crashed with an **"Out of Memory" (F2046)** error after running for **23 minutes**. This indicates that the complex, interface-heavy generic structures of Spring4D, while powerful, put an enormous strain on the compiler when used excessively in a single project.

## Conclusion

For projects requiring a massive number of specialized collection types:
- **StaticGenerics (Code Generation)** and **mORMotGenerics** are the only viable options for maintaining fast iteration cycles and keeping binary sizes low.
- Standard **Delphi Generics** quickly become a bottleneck in both build time and executable size.
- **Spring4D**, despite its features, is not suitable for this specific high-volume scalability scenario due to compiler limitations.
