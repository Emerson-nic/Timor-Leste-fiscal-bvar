import subprocess

from PIL import Image

#make each pdf to png 
subprocess.run([
    'pdftoppm', '-png', '-r', '300', '-singlefile',
    'graphics/irf_gov_shock_order_1.pdf',
    'tmp1'
], check=True)

subprocess.run([
    'pdftoppm', '-png', '-r', '300', '-singlefile',
    'graphics/irf_gov_shock_order_2.pdf',
    'tmp2'
], check=True)

#load png
img1 = Image.open('tmp1.png')
img2 = Image.open('tmp2.png')

#make canvas that is twice as wide 
total_width = img1.width + img2.width
max_height = max(img1.height, img2.height)

combined = Image.new('RGB', (total_width, max_height), (255, 255, 255))
combined.paste(img1, (0, 0))
combined.paste(img2, (img1.width, 0))

#save pdf
combined.save('graphics/irf_gov_shock_combined.pdf', 'PDF', resolution=300.0)
