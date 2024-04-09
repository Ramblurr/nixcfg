export AWS_ACCESS_KEY_ID=volsync-CHANGEME
export AWS_SECRET_ACCESS_KEY='CHANGEME'
export RESTIC_REPOSITORY="s3:https://s3.CHANGEME/volsync-CHANGEME/restic"
export RESTIC_PASSWORD="CHANGEME"
#restic --verbose unlock
restic --no-lock --verbose  restore latest --target ./restored/
