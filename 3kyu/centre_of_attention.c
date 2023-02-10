#include <stdio.h>
#include <stdlib.h>

#define MIN(a,b) ((a) < (b) ? (a) : (b))

typedef struct {
  unsigned *pixels;
  unsigned width;
  unsigned height;
} Image;

typedef struct {
  Image image;
  int max_depth;
  int count_max_depth_pixels;
} depth_buffer;

typedef struct {
  unsigned *values;
  unsigned size;
} unsigned_array;

void print_image(Image *image);

int get_color_by(const Image *image, int x, int y) {
  return image->pixels[y*image->width+x];
}

int update_depth_buffer(depth_buffer *buffer) {
  int set_depth = buffer->max_depth;
  int count_pixels = 0;
  for(int y = 0; y < buffer->image.height; y++) {
    for(int x = 0; x < buffer->image.width; x++) {
      if(get_color_by(&buffer->image, x, y) == 0) {
        if(get_color_by(&buffer->image, x-1, y) == set_depth || get_color_by(&buffer->image, x, y-1) == set_depth ||
           get_color_by(&buffer->image, x+1, y) == set_depth || get_color_by(&buffer->image, x, y+1) == set_depth) {
          buffer->image.pixels[y*buffer->image.width+x] = set_depth+1;
          count_pixels++;
        }
      }
    }
  } 
  if (count_pixels > 0) {
    buffer->max_depth = set_depth+1; 
    return 1;
  }
  return 0;
}

Image create_layout_depth(const Image *image, int color) {
  unsigned *pixels = (unsigned *)malloc(image->width*image->height*sizeof(unsigned));
  Image layout = { pixels, image->width, image->height };
  for(int y = 0; y < image->height; y++) {
    for(int x = 0; x < image->width; x++) {
      int depth = -1;
      if (get_color_by(image, x, y) == color) {
        if((x == 0 || x == image->width-1 || y == 0 || y == image->height-1) || 
          (color != get_color_by(image, x+1, y) || color != get_color_by(image, x, y+1) || 
           color != get_color_by(image, x-1, y) || color != get_color_by(image, x, y-1))) depth = 1;
        else depth = 0;
      }
      *(pixels++) = depth; 
    }
  }
  return layout;
}

depth_buffer get_depth_buffer(Image *image, int color) { 
  Image depth_layout = create_layout_depth(image, color);
  depth_buffer buffer = { depth_layout, 1, 0 };
  
  while(update_depth_buffer(&buffer));
  int count_pixels = 0;
  for(size_t i = 0; i < depth_layout.width * depth_layout.height; i++)
    if(depth_layout.pixels[i] == buffer.max_depth)
      count_pixels++;
  buffer.count_max_depth_pixels = count_pixels;
  
  return buffer;
}

unsigned_array central_pixels(Image image, unsigned color) { 
  depth_buffer depth_map = get_depth_buffer(&image, color);
  unsigned *values = (unsigned *)malloc(depth_map.count_max_depth_pixels*sizeof(unsigned));
  unsigned_array result = { values, depth_map.count_max_depth_pixels };
  
  for(int y = 0; y < depth_map.image.height; y++) {
    for(int x = 0; x < depth_map.image.width; x++) {
      int depth = get_color_by(&depth_map.image, x, y);
      if(depth == depth_map.max_depth)
        *(values++) = y*depth_map.image.width+x;
    }
  }
  return result;
}

void print_image(Image *image) {
  for(int y = 0; y < image->height; y++) {
    for(int x = 0; x < image->width; x++) {
      int color = get_color_by(image, x, y);
      if (color == -1) color = 0;
      printf("%d ", color);
    }
    printf("\n");
  }
}

void do_test(const unsigned_array actual, const unsigned_array expected, const char* test_name) {
  printf("\n%s\n", test_name);
  if (actual.size != expected.size) {
    printf("wrong size:\nexpected size: %d\nbut was: %d\n", expected.size, actual.size);
    return;
  }
  
  for(int i = 0; i < actual.size; i++) {
    printf("%d %d\n", expected.values[i], actual.values[i]);
    if (actual.values[i] != expected.values[i]) { 
      printf("%s wrong:\n  expected: %d\n  but was: %d\n", test_name, expected.values[i], actual.values[i]);
      return;
    }
  }
  printf("\n%s success\n\n", test_name);
}

unsigned image_data[] = {1,1,4,4,4,4,2,2,2,2,
                         1,1,1,1,2,2,2,2,2,2,
                         1,1,1,1,2,2,2,2,2,2,
                         1,1,1,1,1,3,2,2,2,2,
                         1,1,1,1,1,3,3,3,2,2,
                         1,1,1,1,1,1,3,3,3,3};


// only 1 pixel(1) have depth 3
void test1() {
  Image image = { image_data, 10, 6 };
  unsigned red_ctr[] = { 32 };
  unsigned_array expected = { red_ctr, 1 };
  do_test(central_pixels(image, 1), expected, "test1");
}

// multiple pixels(2) have depth 2
void test2() {
  Image image = { image_data, 10, 6 };
  unsigned blue_ctr[] = { 16,17,18,26,27,28,38 };
  unsigned_array expected = { blue_ctr, 7 };
  do_test(central_pixels(image, 2), expected, "test2");
}

// all the pixels(3) have depth 1
void test3() {
  Image image = { image_data, 10, 6 };
  unsigned green_ctr[] = { 35,45,46,47,56,57,58,59 };
  unsigned_array expected = { green_ctr, 8 };
  do_test(central_pixels(image, 3), expected, "test3");
}

// all the pixels(4) have depth 1
void test4() {
  Image image = { image_data, 10, 6 };
  unsigned purple_ctr[] = { 2,3,4,5 };
  unsigned_array expected = { purple_ctr, 4 };
  do_test(central_pixels(image, 4), expected, "test4");
}

// there no pixels with color 5
void test5() {
  Image image = { image_data, 10, 6 };
  unsigned_array expected = { NULL, 0 };
  do_test(central_pixels(image, 5), expected, "test5");
}

void test6() {
  Image image = { image_data, 10, 6 };
  image.pixels[32] = 3;
  unsigned new_ctr[] = { 11,21,41,43 };
  unsigned_array expected = { new_ctr, 4 };
  do_test(central_pixels(image, 1), expected, "test6");
}

void random_test1() {
  unsigned image_data[] = {5, 5, 5, 5, 6, 6, 5, 5, 5, 5, 5, 5, 7, 7, 5, 5, 5, 7, 7, 6, 6, 5, 5, 7, 7, 7, 7, 7, 7,
                           5, 5, 5, 5, 6, 6, 5, 5, 5, 5, 5, 5, 7, 7, 5, 5, 5, 7, 7, 6, 6, 5, 5, 7, 7, 7, 7, 7, 7,
                           5, 5, 5, 5, 6, 6, 5, 5, 5, 5, 5, 5, 7, 7, 5, 5, 5, 7, 7, 6, 6, 5, 5, 7, 7, 7, 7, 7, 7,
                           5, 5, 5, 5, 7, 7, 6, 6, 6, 6, 6, 6, 7, 7, 6, 6, 6, 5, 5, 5, 5, 7, 7, 7, 7, 6, 6, 6, 6,
                           7, 7, 7, 7, 6, 6, 7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 7, 7, 6, 6, 7, 7, 5, 5, 6, 6, 6, 6,
                           7, 7, 7, 7, 6, 6, 7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 7, 7, 6, 6, 7, 7, 5, 5, 6, 6, 6, 6,
                           7, 7, 7, 7, 6, 6, 7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 5, 7, 7, 6, 6, 7, 7, 5, 5, 6, 6, 6, 6,
                           7, 7, 7, 7, 7, 7, 6, 6, 6, 6, 5, 5, 7, 7, 5, 5, 5, 5, 5, 7, 7, 7, 7, 6, 6, 5, 5, 5, 5,
                           7, 7, 7, 7, 7, 7, 6, 6, 6, 6, 5, 5, 7, 7, 5, 5, 5, 5, 5, 7, 7, 7, 7, 6, 6, 5, 5, 5, 5,
                           5, 5, 5, 5, 6, 6, 5, 5, 5, 5, 6, 6, 5, 5, 7, 7, 7, 5, 5, 5, 5, 5, 5, 7, 7, 7, 7, 7, 7,
                           5, 5, 5, 5, 6, 6, 5, 5, 5, 5, 6, 6, 5, 5, 7, 7, 7, 5, 5, 5, 5, 5, 5, 7, 7, 7, 7, 7, 7,
                           5, 5, 5, 5, 6, 6, 5, 5, 5, 5, 6, 6, 5, 5, 7, 7, 7, 5, 5, 5, 5, 5, 5, 7, 7, 7, 7, 7, 7,
                           5, 5, 5, 5, 6, 6, 5, 5, 5, 5, 6, 6, 5, 5, 7, 7, 7, 5, 5, 5, 5, 5, 5, 7, 7, 7, 7, 7, 7,
                           5, 5, 5, 5, 6, 6, 5, 5, 5, 5, 6, 6, 5, 5, 7, 7, 7, 5, 5, 5, 5, 5, 5, 7, 7, 7, 7, 7, 7,
                           7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 7, 7, 6, 6, 7, 7, 7, 7, 7, 6, 6, 7, 7, 6, 6, 7, 7, 7, 7,
                           7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 7, 7, 6, 6, 7, 7, 7, 7, 7, 6, 6, 7, 7, 6, 6, 7, 7, 7, 7,
                           7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 7, 7, 6, 6, 7, 7, 7, 7, 7, 6, 6, 7, 7, 6, 6, 7, 7, 7, 7,
                           6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 7, 7, 5, 5, 6, 6, 6, 6, 6, 7, 7, 5, 5, 6, 6, 5, 5, 5, 5,
                           6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 7, 7, 5, 5, 6, 6, 6, 6, 6, 7, 7, 5, 5, 6, 6, 5, 5, 5, 5,
                           5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 5, 5, 7, 7, 7, 5, 5, 7, 7, 6, 6, 5, 5, 6, 6, 6, 6,
                           5, 5, 5, 5, 7, 7, 7, 7, 7, 7, 5, 5, 7, 7, 5, 5, 5, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5,
                           5, 5, 5, 5, 7, 7, 7, 7, 7, 7, 5, 5, 7, 7, 5, 5, 5, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5,
                           5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 5, 5, 7, 7, 5, 5, 5, 5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 7, 7,
                           5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 5, 5, 7, 7, 5, 5, 5, 5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 7, 7,
                           5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 5, 5, 7, 7, 5, 5, 5, 5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 7, 7,
                           5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 5, 5, 7, 7, 5, 5, 5, 5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 7, 7,
                           7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 6, 6, 7, 7, 7, 7,
                           7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 6, 6, 7, 7, 7, 7, 6, 6, 7, 7, 7, 7};
  Image image = { image_data, 29, 28 };
  print_image(&image);
  unsigned_array pixels = central_pixels(image, 5);
  // for(int i = 0; i < pixels.size; i++)
  //   printf("%d ", pixels.values[i]);
}

void random_test2() {
  unsigned image_data[] = {6,5,5,5,5,5,5,7,7,7,7,5,5,5,5,5,5,7,7,5,5,5,6,6,6,6,6,6,6,
                           6,5,5,5,5,5,5,7,7,7,7,5,5,5,5,5,5,7,7,5,5,5,6,6,6,6,6,6,6,
                           6,5,5,5,5,5,5,7,7,7,7,5,5,5,5,5,5,7,7,5,5,5,6,6,6,6,6,6,6,
                           7,6,6,6,7,7,7,6,6,6,6,7,7,7,7,7,7,5,5,5,5,5,6,6,6,6,6,7,7,
                           7,6,6,6,7,7,7,6,6,6,6,7,7,7,7,7,7,5,5,5,5,5,6,6,6,6,6,7,7,
                           7,6,6,6,7,7,7,6,6,6,6,7,7,7,7,7,7,5,5,5,5,5,6,6,6,6,6,7,7,
                           7,7,7,7,7,7,7,7,7,7,7,6,6,6,7,7,7,6,6,6,6,6,6,6,5,5,5,5,7,
                           7,7,7,7,7,7,7,7,7,7,7,6,6,6,7,7,7,6,6,6,6,6,6,6,5,5,5,5,7,
                           7,7,7,7,7,7,7,7,7,7,7,6,6,6,7,7,7,6,6,6,6,6,6,6,5,5,5,5,7,
                           7,7,7,7,7,7,7,7,7,7,7,6,6,6,7,7,7,6,6,6,6,6,6,6,5,5,5,5,7,
                           7,7,7,7,7,7,7,7,7,7,7,6,6,6,7,7,7,7,7,6,6,6,7,7,6,6,6,7,7,
                           5,6,6,6,5,5,5,5,5,5,5,7,7,7,6,6,6,5,5,5,5,5,7,7,5,5,5,7,5,
                           5,6,6,6,5,5,5,5,5,5,5,7,7,7,6,6,6,5,5,5,5,5,7,7,5,5,5,7,5,
                           5,6,6,6,5,5,5,5,5,5,5,7,7,7,6,6,6,5,5,5,5,5,7,7,5,5,5,7,5,
                           6,7,7,7,7,7,7,6,6,6,6,6,6,6,7,7,7,6,6,6,6,6,6,6,6,6,6,7,7,
                           6,7,7,7,7,7,7,6,6,6,6,6,6,6,7,7,7,6,6,6,6,6,6,6,6,6,6,7,7,
                           6,7,7,7,7,7,7,6,6,6,6,6,6,6,7,7,7,6,6,6,6,6,6,6,6,6,6,7,7,
                           6,7,7,7,7,7,7,6,6,6,6,6,6,6,7,7,7,6,6,6,6,6,6,6,6,6,6,7,7,
                           6,7,7,7,7,7,7,6,6,6,6,6,6,6,7,7,7,6,6,6,6,6,6,6,6,6,6,7,7,
                           6,7,7,7,7,7,7,6,6,6,6,6,6,6,7,7,7,6,6,6,6,6,6,6,6,6,6,7,7,
                           7,5,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,6,6,7,7,7,6,6,6,6,6,5,6,
                           7,5,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,6,6,7,7,7,6,6,6,6,6,5,6,
                           7,5,5,5,5,5,5,6,6,6,6,6,6,6,7,7,7,6,6,7,7,7,6,6,6,6,6,5,6,
                           5,7,7,7,6,6,6,5,5,5,5,5,5,5,6,6,6,7,7,5,5,5,7,7,6,6,6,6,5,
                           5,7,7,7,6,6,6,5,5,5,5,5,5,5,6,6,6,7,7,5,5,5,7,7,6,6,6,6,5,
                           5,7,7,7,6,6,6,5,5,5,5,5,5,5,6,6,6,7,7,5,5,5,7,7,6,6,6,6,5,
                           7,6,6,6,7,7,7,7,7,7,7,6,6,6,6,6,6,6,6,7,7,7,7,7,5,5,5,5,7,
                           7,6,6,6,7,7,7,7,7,7,7,6,6,6,6,6,6,6,6,7,7,7,7,7,5,5,5,5,7,
                           7,6,6,6,6,6,6,7,7,7,7,6,6,6,5,5,5,5,5,5,5,5,6,6,5,5,5,5,6};
  Image image = { image_data, 29, 29 };
  print_image(&image);
  unsigned_array pixels = central_pixels(image, 5);
  unsigned little_of_whats_expected[] = { 809, 808, 716, 708, 707, 706, 705, 704, 614, 613 }; // it's not all
  unsigned_array expected = { little_of_whats_expected, 10 };
  
  printf("size %d\n", pixels.size);
  for(size_t i = 0; i < pixels.size; i++)
    printf("%d ", pixels.values[i]);
  printf("\n");
}

void very_big_test() {
  const size_t width = 4000;
  const size_t height= 4000; 
  unsigned *data = (unsigned *)malloc(width*height*sizeof(unsigned));
  for(size_t i = 0; i < width*height; i++)
    data[i] = 1;
  Image image = { data, width, height };
  unsigned_array pixels = central_pixels(image, 1);
  printf("size: %d\n", pixels.size);
  for(size_t i = 0; i < pixels.size; i++)
    printf("%d ", pixels.values[i]);
}

int main(void) {
  // test1();
  // test2();
  // test3();
  // test4();
  // test5();
  // test6();
  // random_test1();
  // random_test2();
  very_big_test();

  return 0;
}
