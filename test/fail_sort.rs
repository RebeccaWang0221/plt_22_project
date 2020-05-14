def int partition(int arr[6], int low, int high){
  int i = (low-1);         
  int pivot = arr[high];    
  
  for int j in range(low , high){
    if arr[j] < pivot {
      i = i+1; 
      int buf = arr[i]; 
      arr[i] = arr[j];
      arr[j] = buf;
    }      
      
  } 
  int buf2 = arr[i+1]; 
  arr[i+1] = arr[high]; 
  arr[high] = buf2;
  return i+1; 
}

def void quickSort(int arr[6], int low, int high){
  if low < high {
    int pi = partition(arr,low,high); 
    quickSort(arr, low, pi-1); 
    quickSort(arr, pi+1, high);
  }
       
}
    
int arr[] = {10, 7, 8, 9, 1, 5};
int n = 6;
quickSort(arr,0,n-1);
print ("Sorted array is:"); 
for int i in range(n){
  print (arr[i]); 
} 
    
    