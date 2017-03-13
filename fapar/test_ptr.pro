function test_ptr, pointer, var1

 pointer=ptr_new(10, /NO_COPY)
 var1=2

end

pro call_ptr

 pointer=ptr_new(5, /NO_COPY)
 var1=1
 help, *pointer
 help, var1
 a=test_ptr(pointer, var1)
 help, *pointer
 help, var1

end