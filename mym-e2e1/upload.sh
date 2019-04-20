scp -i ../../lckey/cyb1.pem factory.lua ec2-user@cyb1.net:/home/ec2-user/factory.lua
ssh -t -i ../../lckey/cyb1.pem ec2-user@cyb1.net "sudo mv /home/ec2-user/factory.lua /usr/share/nginx/html/oc-scripts/mym-e2e1.lua"
