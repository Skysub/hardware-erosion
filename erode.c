
//New version
void erode(unsigned char input_image[20][20], unsigned char output_image[20][20])
{
    //Setting the cornors to zero
    output_image[0][0] = 0;
    output_image[0][19] = 0;
    output_image[19][0] = 0;
    output_image[19][19] = 0;

    for (int x = 1; x < 19; x++)
    {
        //Setting the edges to zero
        output_image[x][0] = 0;
        output_image[x][19] = 0;
        output_image[0][x] = 0;
        output_image[19][x] = 0;

        for (int y = 1; y < 19; y++)
        {
            if (input_image[x][y] == 255)
            {

                if (input_image[x + 1][y] == 0 ||
                    input_image[x - 1][y] == 0 ||
                    input_image[x][y + 1] == 0 ||
                    input_image[x][y - 1] == 0)
                {
                    output_image[x][y] = 0;
                }
                else {
                    output_image[x][y] = 255;
                }
            }
            else
                output_image[x][y] = 0;
        }
    }
}

//Original version
/*
void erode(unsigned char input_image[20][20], unsigned char output_image[20][20])
{
    output_image[0][0] = 0;
    output_image[0][-1] = 0;
    output_image[-1][0] = 0;
    output_image[-1][-1] = 0;

    for (int x = 1; x < 19; x++)
    {
        output_image[x][0] = 0;
        output_image[x][-1] = 0;
        output_image[0][x] = 0;
        output_image[-1][x] = 0;
     
        for (int y = 1; y < 19; y++)
        {
            if (input_image[x][y] == 255)
            {

                if (input_image[x + 1][y] == 0 ||
                    input_image[x - 1][y] == 0 ||
                    input_image[x][y + 1] == 0 ||
                    input_image[x][y - 1] == 0)
                {
                    output_image[x][y] = 0;
                }
                else {
                    output_image[x][y] = 255;
                }
            }
            else
                output_image[x][y] = 0;
        }
    }
}
*/