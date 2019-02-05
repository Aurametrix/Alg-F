void JumpDownSort(Vector a, int n)
{
    for(int j=n-1; j > 0; j--)
        for(int k=0; k < j; k++)
            if (a[j] < a[k])
                Swap(a,k,j);
}
