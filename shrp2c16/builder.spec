# -*- mode: python -*-
a = Analysis(['C:\\development\\shrp2c16\\branches\\ui-upgrade\\shrp2c16\\gui\\app.py'],
             pathex=['C:\\Python34\\Scripts'],
             hiddenimports=[],
             hookspath=None,
             runtime_hooks=None)
pyz = PYZ(a.pure)
exe = EXE(pyz,
          a.scripts,
          exclude_binaries=True,
          name='rpat.exe',
          debug=True,
          strip=None,
          upx=True,
          console=True )
extra_tree1 = Tree('/development/shrp2c16/branches/ui-upgrade/shrp2c16/gui/', prefix=None, excludes=['*.spec','*.zip','*.py'])
extra_tree2 = Tree('/development/shrp2c16/branches/ui-upgrade/shrp2c16/gui/views', prefix='static', excludes=['tmp'])

coll = COLLECT(exe,
               a.binaries + extra_tree1 + extra_tree2,
               a.zipfiles,
               a.datas,
               strip=None,
               upx=True,
               name='rpat')
