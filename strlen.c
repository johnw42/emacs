
int strlen(const char *s) {
  int n = 0;
  while (*s++ != 0) n++;
  return n;
}


int __strlen_avx2(const char *s) {
  return strlen(s);
}
