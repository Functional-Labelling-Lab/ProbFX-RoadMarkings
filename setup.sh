sudo cp -r external/glad /usr/include/ ; echo "Moved glad to include"
sudo cp -r external/KHR /usr/include/ ; echo "Moved KHR to include"

cp external/glad.c src/ ; echo "Installed glad to source"