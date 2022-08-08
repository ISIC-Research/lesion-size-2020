# Running the Lesion Size 2020 images through ADAE

## Creating environment
First, please create and activate a conda/python (3.9) environment with

```
conda create -y --name ADAE python=3.9
conda activate ADAE
```

Next, please install required python packages with

```
pip install -r requirements.txt
pip install git+https://github.com/ildoonet/pytorch-gradual-warmup-lr.git
git clone https://github.com/NVIDIA/apex
cd apex
pip install ./
cd ..
```

Then please clone the ADAE repository using

```
mkdir code
git clone http://github.com/haqishen/SIIM-ISIC-Melanoma-Classification-1st-Place-Solution/ ./code
chmod 755 code/*.py
```

And then download the pre-trained model weights into the correct folder

```
mkdir code/weights
kaggle d download boliu0/melanoma-winning-models -p code/weights --unzip
```

## Downloading and processing images
Finally, download the images from the ISIC Archive, and create three sized versions

```
isic image download -l 0 -c 162 data
python fudge_metadata.py
cd data
for n in *JPG ; do
    m=`echo $n|sed -e "s/.JPG/.jpg/"`
    mv $n $m
  done
mkdir -p jpeg-melanoma-512x512/test
mogrify -format jpg -path jpeg-melanoma-512x512/test/ -thumbnail 512x512^ -gravity center -extent 512x512 -quality 95 *.jpg
cd jpeg-melanoma-512x512
ln -s ../train.csv .
ln -s ../data.csv test.csv
mkdir train
cd train
for im in ../../ISIC_01* ; do
    ln -s $im .
  done
cd ../..

mkdir -p jpeg-melanoma-768x768/test
mogrify -format jpg -path jpeg-melanoma-768x768/test/ -thumbnail 768x768^ -gravity center -extent 768x768 -quality 95 *.jpg
cd jpeg-melanoma-768x768
ln -s ../train.csv .
ln -s ../data.csv test.csv
mkdir train
cd train
for im in ../../ISIC_01* ; do
    ln -s $im .
  done
cd ../..

mkdir -p jpeg-melanoma-1024x1024/test
mogrify -format jpg -path jpeg-melanoma-1024x1024/test/ -thumbnail 1024x1024^ -gravity center -extent 1024x1024 -quality 95 *.jpg
cd jpeg-melanoma-1024x1024
ln -s ../train.csv .
ln -s ../data.csv test.csv
mkdir train
cd train
for im in ../../ISIC_01* ; do
    ln -s $im .
  done
cd ../..

mkdir jpeg-isic2019-512x512
cd jpeg-isic2019-512x512
ln -s ../train-isic2019.csv train.csv .
mkdir train
cd train
for im in ../../ISIC_01* ; do
    ln -s $im .
  done
cd ../..

mkdir jpeg-isic2019-768x768
cd jpeg-isic2019-768x768
ln -s ../train-isic2019.csv train.csv .
mkdir train
cd train
for im in ../../ISIC_01* ; do
    ln -s $im .
  done
cd ../..

mkdir jpeg-isic2019-1024x1024
cd jpeg-isic2019-1024x1024
ln -s ../train-isic2019.csv train.csv .
mkdir train
cd train
for im in ../../ISIC_01* ; do
    ln -s $im .
  done
cd ../..
```

## Running individual models
To run the individual models, we call the predict.py code with a set of parameters:

```
cd ../code
. ../run_models.sh
```
