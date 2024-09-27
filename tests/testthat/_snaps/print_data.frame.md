# printing abbreviated data.frames works

    Code
      print_data.frame(x, rows = 20, digits = 0)
    Output
         a  b 
      1  -1  1
      2   0  1
      3  -1  0
      4   2 -2
      5   0  1
      6  -1  0
      7   0  0
      8   1 -1
      9   1  0
      10  0  0
      11  2  1
      12  0  0
      13 -1  0
      14 -2  0
      15  1 -1
      16  0  0
      17  0  0
      18  1  0
      19  1  1
      20  1  1

---

    Code
      print_data.frame(x, rows = 7)
    Output
         a           b          
      1  -0.62645381  0.91897737
      2   0.18364332  0.78213630
      3  -0.83562861  0.07456498
      4   1.59528080 -1.98935170
      
    Message
      < 13 rows hidden >
    Output
                                
      18  0.94383621 -0.05931340
      19  0.82122120  1.10002537
      20  0.59390132  0.76317575

---

    Code
      print_data.frame(x, rows = 7, digits = 1)
    Output
         a    b   
      1  -0.6  0.9
      2   0.2  0.8
      3  -0.8  0.1
      4   1.6 -2.0
      
    Message
      < 13 rows hidden >
    Output
                  
      18  0.9 -0.1
      19  0.8  1.1
      20  0.6  0.8

---

    Code
      print_data.frame(x, rows = 7, digits = 1, row.names = FALSE)
    Output
       a    b   
       -0.6  0.9
        0.2  0.8
       -0.8  0.1
        1.6 -2.0
      
    Message
      < 13 rows hidden >
    Output
                
        0.9 -0.1
        0.8  1.1
        0.6  0.8

---

    Code
      print_data.frame(data.frame())
    Message
      < data.frame with 0 columns and 0 rows >

---

    Code
      print_data.frame(data.frame(x = numeric()))
    Message
      < data.frame with 1 column and 0 rows >

