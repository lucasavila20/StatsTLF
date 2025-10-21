package_init <- StatsTLF::create_content_package(
 name = ,
 start_number = list(T = 1L, F = 1L, L = 1L),
 sep_subtitle = "newline",
 sep_population = "newline",
 language = "EN-US"
)

package <- StatsTLF::run_caller_contents(package_init)

StatsTLF::export_report(
 package = package,
 template_name = "template_EN-US.docx",
 supp = FALSE,
 add_toc = FALSE,
 spec = ""
)

rm(list = ls())
