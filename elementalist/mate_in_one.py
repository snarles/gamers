import numpy as np

shuriken = np.array([0,
  1, 1, 2, 3, 5, 1, 6, 0,
  5, 5, 3, 1, 4, 5, 2, 0,
  4, 4, 1, 5, 6, 4, 3, 0,
  6, 6, 5, 4, 2, 6, 1, 0,
  2, 2, 4, 6, 3, 2, 5, 0,
  3, 3, 6, 2, 1, 3, 4
])

# Maximum number of tokens per element
ME = 6
# First dimension: (value, solved vs not, temp variable).
# Fire limited to 2 due to inutility for mate-in-one
dims = (3, 48, 48, ME, 2, ME, ME, ME, ME, 2, 2, 2, 2, 2, 2)
# game values
vals = np.zeros(dims, dtype=np.bool_)

# all losing states
vals[1,:,:,0,:,:,:,:,:,:,:,:,:,:,1] = 1
vals[0,:,:,0,:,:,:,:,:,:,:,:,:,:,1] = 0
vals[1,:,:,:,0,:,:,:,:,:,:,:,:,:,1] = 1
vals[0,:,:,:,0,:,:,:,:,:,:,:,:,:,1] = 0
vals[1,:,:,:,:,0,:,:,:,:,:,:,:,:,1] = 1
vals[0,:,:,:,:,0,:,:,:,:,:,:,:,:,1] = 0
vals[1,:,:,:,:,:,0,:,:,:,:,:,:,:,1] = 1
vals[0,:,:,:,:,:,0,:,:,:,:,:,:,:,1] = 0
vals[1,:,:,:,:,:,:,0,:,:,:,:,:,:,1] = 1
vals[0,:,:,:,:,:,:,0,:,:,:,:,:,:,1] = 0
vals[1,:,:,:,:,:,:,:,0,:,:,:,:,:,1] = 1
vals[0,:,:,:,:,:,:,:,0,:,:,:,:,:,1] = 0

# all winning states
vals[:2, :,:,1:,1:,1:,1:,1:,1:,:,:,:,:,:,:] = 1

(np.sum(vals[0]), np.sum(vals[1]))/np.prod(dims[1:])


#actions = ['Mv','L','A','W','E','Gl','Ga','Gw','Ge']

def action_output(vals,action,i,j,z):
    eval_flag = True
    if action=='Mv' and z==1:
        return 0,0,False
    if action=='Mv' and z==0:
        inew = (i + shuriken[j]) % 48
        if shuriken[inew]==0:
            oldv=vals[:,i,j, :,:,:,:,:,:, :,:,:,:,:,0]
            newv=vals[:,inew,j, :,:,:,:,:,:, :,:,:,:,:,1]
            return oldv,newv,eval_flag
        if shuriken[inew]==1:
            oldv=vals[:,i,j, :-1,:,:,:,:,:, :,:,:,:,:,0]
            newv=vals[:,inew,j, 1:,:,:,:,:,:, :,:,:,:,:,1]
            return oldv,newv,eval_flag
        if shuriken[inew]==2:
            oldv=vals[:,i,j, :,:-1,:,:,:,:, :,:,:,:,:,0]
            newv=vals[:,inew,j, :,1:,:,:,:,:, :,:,:,:,:,1]
            return oldv,newv,eval_flag
        if shuriken[inew]==3:
            oldv=vals[:,i,j, :,:,:-1,:,:,:, :,:,:,:,:,0]
            newv=vals[:,inew,j, :,:,1:,:,:,:, :,:,:,:,:,1]
            return oldv,newv,eval_flag
        if shuriken[inew]==4:
            oldv=vals[:,i,j, :,:,:,:-1,:,:, :,:,:,:,:,0]
            newv=vals[:,inew,j, :,:,:,1:,:,:, :,:,:,:,:,1]
            return oldv,newv,eval_flag
        if shuriken[inew]==5:
            oldv=vals[:,i,j, :,:,:,:,:-1,:, :,:,:,:,:,0]
            newv=vals[:,inew,j, :,:,:,:,1:,:, :,:,:,:,:,1]
            return oldv,newv,eval_flag
        if shuriken[inew]==6:
            oldv=vals[:,i,j, :,:,:,:,:,:-1, :,:,:,:,:,0]
            newv=vals[:,inew,j, :,:,:,:,:,1:, :,:,:,:,:,1]
            return oldv,newv,eval_flag
    if action=='L':
        oldv = vals[:, i,j, 1:,:,:,:,:,:, z,:,:,:,:,0]
        newv = vals[:, j,i, :-1,:,:,:,:,:, 1,:,:,:,:,0]
        return oldv,newv,eval_flag
    if action=='A':
        newj = (j + 1) % 48
        if (newj % 8)==0:
            newj = (newj + 1) % 48
        oldv = vals[:, i,j, :,:,1:,:,:,:, :,:,z,:,:,0]
        newv = vals[:, i,newj, :,:,:-1,:,:,:, :,:,1,:,:,0]
        return oldv,newv,eval_flag
    if action=='W':
        newi = (i + 1) % 48
        if (newi % 8)==0:
            newi = (newi + 1) % 48
        oldv = vals[:, i,j, :,:,:,1:,:,:, :,:,:,z,:,0]
        newv = vals[:, newi,j, :,:,:,:-1,:,:, :,:,:,1,:,0]
        return oldv,newv,eval_flag
    if action=='E':
        if shuriken[i]==0:
            oldv=vals[:,i,j, :,:,:,:,1:,:, :,:,:,:,z,0]
            newv=vals[:,i,j, :,:,:,:,:-1,:, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==1:
            oldv=vals[:,i,j, :-1,:,:,:,1:,:, :,:,:,:,z,0]
            newv=vals[:,i,j, 1:,:,:,:,:-1,:, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==2:
            oldv=vals[:,i,j, :,:-1,:,:,1:,:, :,:,:,:,z,0]
            newv=vals[:,i,j, :,1:,:,:,:-1,:, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==3:
            oldv=vals[:,i,j, :,:,:-1,:,1:,:, :,:,:,:,z,0]
            newv=vals[:,i,j, :,:,1:,:,:-1,:, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==4:
            oldv=vals[:,i,j, :,:,:,:-1,1:,:, :,:,:,:,z,0]
            newv=vals[:,i,j, :,:,:,1:,:-1,:, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==5 and z==0:
            oldv=vals[:,i,j, :,:,:,:,:,:, :,:,:,:,z,0]
            newv=vals[:,i,j, :,:,:,:,:,:, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==5 and z==1:
            # to prevent self-loop
            return 0,0,False
        if shuriken[i]==6:
            oldv=vals[:,i,j, :,:,:,:,1:,:-1, :,:,:,:,z,0]
            newv=vals[:,i,j, :,:,:,:,:-1,1:, :,:,:,:,1,0]
            return oldv,newv,eval_flag
    if z==1:
        # Should only occur for Grass moves
        return 0,0,False
    if action=='Gl':
        oldv = vals[:, i,j, :,:,:,:,:,1:, 1,:,:,:,:,0]
        newv = vals[:, j,i, :,:,:,:,:,:-1, 1,:,:,:,:,0]
        return oldv,newv,eval_flag
    if action=='Ga':
        newj = (j + 1) % 48
        if (newj % 8)==0:
            newj = (newj + 1) % 48
        oldv = vals[:, i,j, :,:,:,:,:,1:, :,:,1,:,:,0]
        newv = vals[:, i,newj, :,:,:,:,:,:-1, :,:,1,:,:,0]
        return oldv,newv,eval_flag
    if action=='Gw':
        newi = (i + 1) % 48
        if (newi % 8)==0:
            newi = (newi + 1) % 48
        oldv = vals[:, i,j, :,:,:,:,:,1:, :,:,:,1,:,0]
        newv = vals[:, newi,j, :,:,:,:,:,:-1, :,:,:,1,:,0]
        return oldv,newv,eval_flag
    if action=='Ge':
        if shuriken[i]==0:
            oldv=vals[:,i,j, :,:,:,:,:,1:, :,:,:,:,1,0]
            newv=vals[:,i,j, :,:,:,:,:,:-1, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==1:
            oldv=vals[:,i,j, :-1,:,:,:,:,1:, :,:,:,:,1,0]
            newv=vals[:,i,j, 1:,:,:,:,:,:-1, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==2:
            oldv=vals[:,i,j, :,:-1,:,:,:,1:, :,:,:,:,1,0]
            newv=vals[:,i,j, :,1:,:,:,:,:-1, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==3:
            oldv=vals[:,i,j, :,:,:-1,:,:,1:, :,:,:,:,1,0]
            newv=vals[:,i,j, :,:,1:,:,:,:-1, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==4:
            oldv=vals[:,i,j, :,:,:,:-1,:,1:, :,:,:,:,1,0]
            newv=vals[:,i,j, :,:,:,1:,:,:-1, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==5:
            oldv=vals[:,i,j, :,:,:,:,:-1,1:, :,:,:,:,1,0]
            newv=vals[:,i,j, :,:,:,:,1:,:-1, :,:,:,:,1,0]
            return oldv,newv,eval_flag
        if shuriken[i]==6:
            # to prevent self-loop
            return 0,0,False

while (np.sum(vals[1]==0) > 0):
    # set to 1 to check if all visited states were solved
    vals[2] = 1
    actions = ['Mv','L','A','W','E','Gl','Ga','Gw','Ge']
    for action in actions:
        for i in range(48):
            for j in range(48):
                for z in range(2):
                    oldv,newv,eval_flag = action_output(vals,action,i,j,z)
                    if eval_flag:
                        # propagate wins which are also solved
                        larger = newv[0] > oldv[0]
                        oldv[0][larger] = 1
                        oldv[1][larger] = 1
                        # set temp variable to 0 for unsolved values
                        oldv[2][newv[1] == 0] = 0
    # If all visited states are solved and the value is still zero
    vals[1][np.logical_and(vals[0]==0, vals[2]==1)]=1
    print((np.sum(vals[0]), np.sum(vals[1]))/np.prod(dims[1:]))



np.nonzero(vals[0, 1,:, 1,0,1,0,1,5, 0,0,0,0,0,0]) # 13

np.save('mi1.npy', vals[0][:,:, :,:,:,:,:,:, 0,0,0,0,0,0])
