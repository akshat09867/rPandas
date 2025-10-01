data("diamonds")   
r_df <- diamonds  
translated <- translate_filter(carat > 1 & cut == "Ideal")
py_statement <- create_pandas_statement("r.r_df", t)
exe <- execute_pandas_statement(r_df, c, "code")
print(head(exe))