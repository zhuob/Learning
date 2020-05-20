generate_html_deck = function(title = "RMarkdown Example", outfile = tempfile(pattern = "RMarkdown_Example_", fileext = ".html"), markdown_params = list(), open_in_browser = FALSE) {
    
    tryCatch({
        
        render_rmarkdown(
            rmd_file_path   = "Example_Markdown.Rmd",
            output_file     = outfile,
            title           = title,
            markdown_params = markdown_params
        )
        
        if (open_in_browser) {
            utils::browseURL(outfile)
        }
        
    }, error = function(e){
        devnull <- knitr::knit_meta(class = NULL, clean = TRUE)
    })
    
    return(outfile)
}

render_rmarkdown <- function(rmd_file_path, output_file, title, markdown_params = list(), workdir = getwd() ){
    
    rmarkdown::render(
        input             = rmd_file_path,
        intermediates_dir = workdir,
        output_file       = output_file,
        params            = markdown_params,
        output_options    = list(pandoc_args = c(sprintf('--metadata=title:"%s"', title)))
    )
}
